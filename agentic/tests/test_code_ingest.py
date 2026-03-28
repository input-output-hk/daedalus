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

            source_paths = code.discover_code_source_paths(workspace)

        self.assertEqual(
            source_paths,
            [
                "source/common/ipc/api.ts",
                "source/main/ipc/cardano.ipc.ts",
                "source/renderer/app/components/View.tsx",
                "source/renderer/app/stores/Store.ts",
            ],
        )

    def test_helper_contracts_cover_language_preview_ids_and_hashes(self):
        self.assertTrue(code.is_supported_code_path("source/common/ipc/api.ts"))
        self.assertTrue(code.is_supported_code_path("source/renderer/app/components/View.tsx"))
        self.assertFalse(code.is_supported_code_path("source/common/ipc/api.d.ts"))
        self.assertFalse(code.is_supported_code_path("source/renderer/app/theme.scss.d.ts"))
        self.assertEqual(code.classify_code_language("source/common/ipc/api.ts"), "typescript")
        self.assertEqual(
            code.classify_code_language("source/renderer/app/components/View.tsx"),
            "typescriptreact",
        )
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
