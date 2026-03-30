from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from agentic_kb.ingest import code


REPO_ROOT = Path(__file__).resolve().parents[2]


class FakeEmbeddingClient:
    def __init__(self):
        self.calls: list[list[str]] = []

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        self.calls.append(list(texts))
        return [[float(index)] * 384 for index, _ in enumerate(texts, start=1)]


class CodeIngestTests(unittest.TestCase):
    def test_discover_code_source_paths_matches_allowlist_and_excludes_declarations(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/ipc/api.ts", "export const API = 'api';\n")
            self._write_file(workspace / "source/main/ipc/cardano.ipc.ts", "export const main = 1;\n")
            self._write_file(workspace / "source/renderer/app/stores/Store.ts", "export const store = 1;\n")
            self._write_file(workspace / "source/renderer/app/components/View.tsx", "export function View() { return <div />; }\n")
            self._write_file(workspace / "source/renderer/app/types/generated.d.ts", "export type Ghost = string;\n")
            self._write_file(workspace / "source/renderer/app/types/theme.scss.d.ts", "declare const styles: string;\n")
            self._write_file(workspace / "storybook/Button.tsx", "export const Button = () => null;\n")
            self._write_file(workspace / ".buildkite/pipeline.yml", "steps:\n  - label: test\n")
            self._write_file(workspace / "translations/en-US.json", '{"hello":"world"}\n')
            self._write_file(workspace / "agentic/Dockerfile", "FROM python:3.12-slim\n")
            self._write_file(workspace / ".eslintrc", '{"root":true}\n')
            self._write_file(workspace / "LICENSE", "license text\n")
            self._write_file(workspace / "logs/app.log", "skip me\n")
            self._write_file(workspace / "source/node_modules/pkg/index.js", "export const bad = 1;\n")

            source_paths = code.discover_code_source_paths(workspace)

        self.assertIn("source/common/ipc/api.ts", source_paths)
        self.assertIn("source/main/ipc/cardano.ipc.ts", source_paths)
        self.assertIn("source/renderer/app/components/View.tsx", source_paths)
        self.assertIn("source/renderer/app/stores/Store.ts", source_paths)
        self.assertIn("storybook/Button.tsx", source_paths)
        self.assertIn(".buildkite/pipeline.yml", source_paths)
        self.assertIn("translations/en-US.json", source_paths)
        self.assertIn("agentic/Dockerfile", source_paths)
        self.assertIn(".eslintrc", source_paths)
        self.assertNotIn("LICENSE", source_paths)
        self.assertNotIn("logs/app.log", source_paths)
        self.assertNotIn("source/node_modules/pkg/index.js", source_paths)
        self.assertNotIn("source/renderer/app/types/generated.d.ts", source_paths)
        self.assertNotIn("source/renderer/app/types/theme.scss.d.ts", source_paths)

    def test_helper_contracts_cover_language_preview_ids_and_hashes(self):
        self.assertTrue(code.is_supported_code_path("source/common/ipc/api.ts"))
        self.assertTrue(code.is_supported_code_path("source/renderer/app/components/View.tsx"))
        self.assertTrue(code.is_supported_code_path("agentic/Dockerfile"))
        self.assertTrue(code.is_supported_code_path(".eslintrc"))
        self.assertFalse(code.is_supported_code_path("source/common/ipc/api.d.ts"))
        self.assertFalse(code.is_supported_code_path("source/renderer/app/theme.scss.d.ts"))
        self.assertFalse(code.is_supported_code_path("LICENSE"))
        self.assertFalse(code.is_supported_code_path("foo/build/output.js"))
        self.assertEqual(code.classify_code_language("source/common/ipc/api.ts"), "typescript")
        self.assertEqual(
            code.classify_code_language("source/renderer/app/components/View.tsx"),
            "typescriptreact",
        )
        self.assertEqual(code.classify_code_language("agentic/Dockerfile"), "config")
        self.assertEqual(code.classify_code_language("translations/messages.json"), "json")
        self.assertEqual(
            code.deterministic_code_chunk_id("source/common/ipc/api.ts", chunk_index=7),
            "code:source/common/ipc/api.ts#7",
        )
        self.assertEqual(
            code.build_code_preview_text("\nconst alpha = 1;\n\nconst beta = 2;\n"),
            "const alpha = 1; const beta = 2;",
        )
        self.assertNotEqual(
            code.deterministic_code_content_hash(
                "source/common/ipc/api.ts",
                symbol_name="Foo",
                symbol_kind="function",
                parent_symbol_name=None,
                parent_symbol_kind=None,
                content="export function Foo() {}",
            ),
            code.deterministic_code_content_hash(
                "source/common/ipc/api.ts",
                symbol_name="Bar",
                symbol_kind="function",
                parent_symbol_name=None,
                parent_symbol_kind=None,
                content="export function Foo() {}",
            ),
        )

    def test_extract_typescript_symbol_chunks_supports_export_lists_and_trailing_default_exports(self):
        analytics_content = (REPO_ROOT / "source/renderer/app/components/analytics/AnalyticsProvider.tsx").read_text(
            encoding="utf-8"
        )
        analytics_symbols = code.extract_typescript_symbol_chunks(
            "source/renderer/app/components/analytics/AnalyticsProvider.tsx",
            analytics_content,
        )

        analytics_provider = next(
            symbol for symbol in analytics_symbols if symbol.symbol_name == "AnalyticsProvider"
        )
        self.assertEqual(analytics_provider.symbol_kind, "function")
        self.assertFalse(analytics_provider.metadata["is_default_export"])
        self.assertTrue(analytics_provider.metadata["exported_via_list"])
        self.assertEqual(analytics_provider.start_line, 10)
        self.assertEqual(analytics_provider.end_line, 16)

        mithril_content = (REPO_ROOT / "source/renderer/app/containers/loading/MithrilBootstrapPage.tsx").read_text(
            encoding="utf-8"
        )
        mithril_symbols = code.extract_typescript_symbol_chunks(
            "source/renderer/app/containers/loading/MithrilBootstrapPage.tsx",
            mithril_content,
        )

        mithril_page = next(
            symbol for symbol in mithril_symbols if symbol.symbol_name == "MithrilBootstrapPage"
        )
        self.assertEqual(mithril_page.symbol_kind, "class")
        self.assertTrue(mithril_page.metadata["is_default_export"])
        self.assertFalse(mithril_page.metadata["exported_via_list"])

    def test_extract_typescript_symbol_chunks_supports_exported_destructuring_variables(self):
        content = (
            "const launcherConfig = loadConfig();\n"
            "export const { cluster, stateDir: stateDirectoryPath, ...rest } = launcherConfig;\n"
            "export const [firstValue, { nested: secondValue = 2 }] = values;\n"
        )

        symbols = code.extract_typescript_symbol_chunks(
            "source/main/destructuring.ts",
            content,
        )

        self.assertEqual(
            [symbol.symbol_name for symbol in symbols],
            ["cluster", "stateDirectoryPath", "rest", "firstValue", "secondValue"],
        )
        self.assertTrue(all(symbol.symbol_kind == "variable" for symbol in symbols))
        self.assertTrue(all("export const" in symbol.content for symbol in symbols))

    def test_prepare_code_chunks_captures_real_destructured_exports_from_source_main_config(self):
        embedding_client = FakeEmbeddingClient()
        prepared = code.prepare_code_chunks(
            REPO_ROOT,
            source_paths=["source/main/config.ts"],
            embedding_client=embedding_client,
            repo_commit_hash="config-commit",
        )

        destructured_chunks = {
            chunk.symbol_name: chunk
            for chunk in prepared
            if chunk.start_line == 116 and chunk.symbol_kind == "variable"
        }

        self.assertTrue(
            {"cluster", "nodeImplementation", "stateDir", "legacyStateDir", "logsPrefix", "isFlight", "smashUrl"}
            .issubset(destructured_chunks)
        )
        self.assertTrue(
            all(chunk.repo_path == "source/main/config.ts" for chunk in destructured_chunks.values())
        )
        self.assertTrue(
            all("launcherConfig" in chunk.content for chunk in destructured_chunks.values())
        )
        self.assertTrue(all(chunk.metadata["symbol_path"] == chunk.symbol_name for chunk in destructured_chunks.values()))

    def test_extract_typescript_symbol_chunks_extracts_exported_class_members(self):
        content = (REPO_ROOT / "source/renderer/app/stores/TransactionsStore.ts").read_text(
            encoding="utf-8"
        )
        symbols = code.extract_typescript_symbol_chunks(
            "source/renderer/app/stores/TransactionsStore.ts",
            content,
        )

        transactions_store = next(
            symbol for symbol in symbols if symbol.symbol_name == "TransactionsStore"
        )
        self.assertEqual(transactions_store.symbol_kind, "class")
        self.assertTrue(transactions_store.metadata["is_default_export"])
        self.assertTrue(transactions_store.metadata["has_member_chunks"])

        refresh_member = next(
            symbol
            for symbol in symbols
            if symbol.symbol_name == "_refreshTransactionData"
            and symbol.parent_symbol_name == "TransactionsStore"
        )
        self.assertEqual(refresh_member.symbol_kind, "field_function")
        self.assertEqual(refresh_member.parent_symbol_kind, "class")
        self.assertEqual(
            refresh_member.metadata["symbol_path"],
            "TransactionsStore._refreshTransactionData",
        )
        self.assertEqual(refresh_member.start_line, 260)
        self.assertGreater(refresh_member.end_line, refresh_member.start_line)

        getter_member = next(
            symbol
            for symbol in symbols
            if symbol.symbol_name == "recentTransactionsRequest"
            and symbol.parent_symbol_name == "TransactionsStore"
        )
        self.assertEqual(getter_member.symbol_kind, "getter")

    def test_prepare_code_chunks_shapes_rows_for_representative_repo_files(self):
        embedding_client = FakeEmbeddingClient()
        prepared = code.prepare_code_chunks(
            REPO_ROOT,
            source_paths=[
                "source/common/ipc/api.ts",
                "source/main/ipc/cardano.ipc.ts",
                "source/renderer/app/stores/TransactionsStore.ts",
                "source/renderer/app/components/analytics/AnalyticsProvider.tsx",
                "source/renderer/app/containers/loading/MithrilBootstrapPage.tsx",
            ],
            embedding_client=embedding_client,
            repo_commit_hash="abc123",
        )

        api_chunk = next(
            chunk
            for chunk in prepared
            if chunk.repo_path == "source/common/ipc/api.ts"
            and chunk.symbol_name == "GET_LOGS_CHANNEL"
        )
        self.assertEqual(api_chunk.language, "typescript")
        self.assertEqual(api_chunk.symbol_kind, "variable")
        self.assertEqual(api_chunk.repo_commit_hash, "abc123")
        self.assertEqual(api_chunk.chunk_index, 0)
        self.assertEqual(api_chunk.start_line, 89)
        self.assertIn("GET_LOGS_CHANNEL", api_chunk.content)
        self.assertEqual(api_chunk.metadata["symbol_path"], "GET_LOGS_CHANNEL")
        self.assertEqual(len(api_chunk.embedding), 384)

        analytics_chunk = next(
            chunk
            for chunk in prepared
            if chunk.repo_path
            == "source/renderer/app/components/analytics/AnalyticsProvider.tsx"
            and chunk.symbol_name == "AnalyticsProvider"
        )
        self.assertEqual(analytics_chunk.language, "typescriptreact")
        self.assertTrue(analytics_chunk.metadata["exported_via_list"])

        member_chunk = next(
            chunk
            for chunk in prepared
            if chunk.repo_path == "source/renderer/app/stores/TransactionsStore.ts"
            and chunk.symbol_name == "calculateTransactionFee"
            and chunk.parent_symbol_name == "TransactionsStore"
        )
        self.assertEqual(member_chunk.parent_symbol_kind, "class")
        self.assertEqual(member_chunk.symbol_kind, "field_function")
        self.assertEqual(
            member_chunk.metadata["symbol_path"],
            "TransactionsStore.calculateTransactionFee",
        )

        self.assertGreaterEqual(sum(len(call) for call in embedding_client.calls), len(prepared))

    def test_prepare_code_chunks_uses_fallback_chunks_for_supported_non_symbol_files(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "translations/en-US.json",
                '{\n  "welcome": "hello",\n  "bye": "goodbye"\n}\n',
            )
            self._write_file(workspace / "agentic/Dockerfile", "FROM python:3.12-slim\nRUN pip install .\n")

            embedding_client = FakeEmbeddingClient()
            prepared = code.prepare_code_chunks(
                workspace,
                source_paths=["translations/en-US.json", "agentic/Dockerfile"],
                embedding_client=embedding_client,
                repo_commit_hash="repo-fallback",
            )

        json_chunk = next(chunk for chunk in prepared if chunk.repo_path == "translations/en-US.json")
        docker_chunk = next(chunk for chunk in prepared if chunk.repo_path == "agentic/Dockerfile")
        self.assertEqual(json_chunk.symbol_kind, "file_chunk")
        self.assertIsNone(json_chunk.symbol_name)
        self.assertEqual(json_chunk.language, "json")
        self.assertEqual(json_chunk.metadata["chunk_strategy"], "fallback_text")
        self.assertEqual(json_chunk.metadata["fallback_reason"], "non_symbol_aware_family")
        self.assertEqual(docker_chunk.language, "config")
        self.assertEqual(docker_chunk.metadata["chunk_strategy"], "fallback_text")

    def test_extract_code_symbols_falls_back_when_parser_errors(self):
        symbols = code.extract_code_symbol_chunks(
            "source/main/broken.js",
            "export function broken( {\n",
        )

        self.assertEqual(len(symbols), 1)
        self.assertEqual(symbols[0].symbol_kind, "file_chunk")
        self.assertEqual(symbols[0].metadata["fallback_reason"], "parse_error")

    def test_build_fallback_text_chunks_handles_long_lines_and_overlap(self):
        long_line = "x" * 6100
        content = "\n".join([long_line, "line-2", "line-3", "line-4"])

        chunks = code.build_fallback_text_chunks(
            "translations/long.json",
            content,
            reason="non_symbol_aware_family",
        )

        self.assertEqual(len(chunks), 2)
        self.assertEqual(chunks[0].start_line, 1)
        self.assertEqual(chunks[0].end_line, 1)
        self.assertEqual(chunks[1].start_line, 2)
        self.assertEqual(chunks[1].metadata["chunk_strategy"], "fallback_text")

    def test_ingest_code_full_repository_prunes_missing_paths_only_when_requested(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._write_file(workspace / "translations/en-US.json", '{"hello":"world"}\n')

            embedding_client = FakeEmbeddingClient()
            store = code.InMemoryCodeChunksStore()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                run_mode="full_repository",
                prune_missing=True,
                repo_commit_hash="full-a",
            )

            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 2;\n")
            (workspace / "source/common/alpha.ts").unlink()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                run_mode="full_repository",
                prune_missing=True,
                repo_commit_hash="full-b",
            )

            remaining_paths = {key[0] for key in store.rows_by_key}

        self.assertIn("source/common/beta.ts", remaining_paths)
        self.assertIn("translations/en-US.json", remaining_paths)
        self.assertNotIn("source/common/alpha.ts", remaining_paths)

    def test_ingest_code_targeted_run_does_not_prune_unrelated_rows(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 2;\n")

            embedding_client = FakeEmbeddingClient()
            store = code.InMemoryCodeChunksStore()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                run_mode="full_repository",
                prune_missing=True,
                repo_commit_hash="seed",
            )

            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 3;\n")
            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/common/beta.ts"],
                run_mode="targeted",
                prune_missing=False,
                repo_commit_hash="targeted",
            )

            remaining_paths = {key[0] for key in store.rows_by_key}

        self.assertIn("source/common/alpha.ts", remaining_paths)
        self.assertIn("source/common/beta.ts", remaining_paths)

    def test_ingest_code_rejects_prune_missing_without_full_repository_mode(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/example.ts", "export const Alpha = 1;\n")

            with self.assertRaisesRegex(ValueError, "prune_missing requires"):
                code.ingest_code(
                    workspace,
                    embedding_client=FakeEmbeddingClient(),
                    code_store=code.InMemoryCodeChunksStore(),
                    source_paths=["source/common/example.ts"],
                    run_mode="targeted",
                    prune_missing=True,
                    repo_commit_hash="bad",
                )

    def test_validate_run_request_rejects_full_repository_mode_with_explicit_source_paths(self):
        with self.assertRaisesRegex(ValueError, "source_paths require run_mode='targeted'"):
            code._validate_run_request(
                source_paths=["source/common/example.ts"],
                options=code.CodeDiscoveryOptions(
                    run_mode="full_repository",
                    prune_missing=True,
                ),
            )

    def test_prepare_code_chunks_rejects_full_repository_mode_with_explicit_source_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/example.ts", "export const Alpha = 1;\n")

            with self.assertRaisesRegex(ValueError, "source_paths require run_mode='targeted'"):
                code._resolve_source_paths(
                    workspace,
                    ["source/common/example.ts"],
                    options=code.CodeDiscoveryOptions(
                        run_mode="full_repository",
                        prune_missing=False,
                    ),
                )

    def test_ingest_code_rejects_full_repository_mode_with_explicit_source_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/example.ts", "export const Alpha = 1;\n")

            with self.assertRaisesRegex(ValueError, "source_paths require run_mode='targeted'"):
                code.ingest_code(
                    workspace,
                    embedding_client=FakeEmbeddingClient(),
                    code_store=code.InMemoryCodeChunksStore(),
                    source_paths=["source/common/example.ts"],
                    run_mode="full_repository",
                    prune_missing=True,
                    repo_commit_hash="bad",
                )

    def test_prepare_code_chunks_skips_oversize_files_during_full_repository_discovery(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/small.ts", "export const Alpha = 1;\n")
            self._write_file(
                workspace / "translations/oversize.json",
                '{"blob":"' + ("x" * (code.CODE_MAX_FILE_BYTES + 10)) + '"}\n',
            )

            prepared = code.prepare_code_chunks(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="oversize",
            )

        repo_paths = {chunk.repo_path for chunk in prepared}
        self.assertIn("source/common/small.ts", repo_paths)
        self.assertNotIn("translations/oversize.json", repo_paths)

    def test_full_repository_discovery_excludes_oversize_files(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/small.ts", "export const Alpha = 1;\n")
            self._write_file(
                workspace / "translations/oversize.json",
                '{"blob":"' + ("x" * (code.CODE_MAX_FILE_BYTES + 10)) + '"}\n',
            )

            discovered = code.discover_code_source_paths(workspace)

        self.assertIn("source/common/small.ts", discovered)
        self.assertNotIn("translations/oversize.json", discovered)

    def test_build_fallback_text_chunks_raises_when_chunk_limit_exceeded(self):
        line = "x" * (code.FALLBACK_MAX_CHARS - 1)
        content = "\n".join(line for _ in range(code.FALLBACK_MAX_CHUNKS_PER_FILE + 5))

        with self.assertRaisesRegex(ValueError, "Fallback chunk count exceeded"):
            code.build_fallback_text_chunks(
                "translations/many-lines.json",
                content,
                reason="non_symbol_aware_family",
            )

    def test_ingest_code_skips_whitespace_only_fallback_chunks(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "translations/blank.json", "   \n\n\t\n")
            store = code.InMemoryCodeChunksStore()
            progress_events = []

            result = code.ingest_code(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                code_store=store,
                source_paths=["translations/blank.json"],
                repo_commit_hash="blank-json",
                progress_callback=lambda current, total, repo_path: progress_events.append((current, total, repo_path)),
            )

        self.assertEqual(result.processed_file_count, 1)
        self.assertEqual(result.chunk_count, 0)
        self.assertEqual(store.rows_by_key, {})
        self.assertEqual(progress_events, [(1, 1, "translations/blank.json")])

    def test_full_repository_prune_removes_rows_when_path_becomes_oversize(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            target = workspace / "translations/en-US.json"
            self._write_file(target, '{"hello":"world"}\n')

            embedding_client = FakeEmbeddingClient()
            store = code.InMemoryCodeChunksStore()

            first_result = code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                run_mode="full_repository",
                prune_missing=True,
                repo_commit_hash="before-oversize",
            )

            self._write_file(
                target,
                '{"blob":"' + ("x" * (code.CODE_MAX_FILE_BYTES + 10)) + '"}\n',
            )

            second_result = code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                run_mode="full_repository",
                prune_missing=True,
                repo_commit_hash="after-oversize",
            )

        self.assertEqual(first_result.processed_file_count, 1)
        self.assertEqual(second_result.processed_file_count, 0)
        self.assertFalse(store.rows_by_key)

    def test_ingest_code_replaces_chunks_for_a_file_on_reingest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            target = workspace / "source/common/example.ts"
            self._write_file(
                target,
                "export const Alpha = 1;\nexport function Beta() {\n  return Alpha;\n}\n",
            )

            embedding_client = FakeEmbeddingClient()
            store = code.InMemoryCodeChunksStore()

            first_result = code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/common/example.ts"],
                repo_commit_hash="commit-a",
            )

            first_symbols = {
                row["symbol_name"]
                for key, row in store.rows_by_key.items()
                if key[0] == "source/common/example.ts"
            }

            self._write_file(
                target,
                "export const Gamma = 2;\n",
            )

            second_result = code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/common/example.ts"],
                repo_commit_hash="commit-b",
            )

        self.assertEqual(first_result.processed_file_count, 1)
        self.assertEqual(second_result.processed_file_count, 1)
        self.assertEqual(first_result.chunk_count, 2)
        self.assertEqual(second_result.chunk_count, 1)
        self.assertEqual(first_symbols, {"Alpha", "Beta"})

        second_rows = {
            key: row
            for key, row in store.rows_by_key.items()
            if key[0] == "source/common/example.ts"
        }
        self.assertEqual(set(second_rows), {("source/common/example.ts", 0)})
        self.assertEqual(second_rows[("source/common/example.ts", 0)]["symbol_name"], "Gamma")
        self.assertEqual(second_rows[("source/common/example.ts", 0)]["repo_commit_hash"], "commit-b")

    def test_ingest_code_reports_file_progress(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 2;\n")
            store = code.InMemoryCodeChunksStore()
            progress_events = []

            result = code.ingest_code(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                code_store=store,
                source_paths=["source/common/alpha.ts", "source/common/beta.ts"],
                repo_commit_hash="abc123",
                progress_callback=lambda current, total, repo_path: progress_events.append((current, total, repo_path)),
            )

        self.assertEqual(result.processed_file_count, 2)
        self.assertEqual(
            progress_events,
            [
                (1, 2, "source/common/alpha.ts"),
                (2, 2, "source/common/beta.ts"),
            ],
        )

    def test_parser_only_allowlist_sweep_succeeds_for_task_401_scope(self):
        source_paths = code.discover_code_source_paths(REPO_ROOT)
        parsed_paths = code.parse_codebase_source_paths(REPO_ROOT, source_paths=source_paths)

        self.assertEqual(tuple(source_paths), parsed_paths)
        self.assertGreater(len(parsed_paths), 0)

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


if __name__ == "__main__":
    unittest.main()
