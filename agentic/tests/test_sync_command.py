from __future__ import annotations

import io
import subprocess
import tempfile
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

from agentic_kb import cli
from agentic_kb.commands import sync
from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import docs, github, project
from agentic_kb.snapshot_manifest import SNAPSHOT_EMBEDDING_CONTRACT_ID, SnapshotManifestRecord
from agentic_kb.sync.state import (
    InMemorySyncStateStore,
    PreparedSyncState,
    deterministic_sync_state_id,
    github_scope_key,
    project_scope_key,
    repo_scope_key,
)

try:
    from agentic_kb.ingest import code
except ImportError:  # pragma: no cover
    code = None


class FakeEmbeddingClient:
    def __init__(self):
        self.calls = []

    def embed_texts(self, texts):
        self.calls.append(list(texts))
        return [[1.0] * 384 for _ in texts]


class SyncCommandTests(unittest.TestCase):
    def test_cli_routes_all_sync_verbs_to_real_handlers(self):
        parser = cli.build_parser()

        self.assertIs(parser.parse_args(["sync", "all"]).handler, sync.run_sync_all)
        self.assertIs(parser.parse_args(["sync", "docs"]).handler, sync.run_sync_docs)
        self.assertIs(parser.parse_args(["sync", "code"]).handler, sync.run_sync_code)
        self.assertIs(parser.parse_args(["sync", "github"]).handler, sync.run_sync_github)
        self.assertIs(parser.parse_args(["sync", "project"]).handler, sync.run_sync_project)
        self.assertIs(parser.parse_args(["sync", "changed"]).handler, sync.run_sync_changed)

    def test_load_required_incremental_baselines_rejects_missing_code_row(self):
        store = InMemorySyncStateStore()
        store.upsert_sync_states([
            PreparedSyncState(
                id=deterministic_sync_state_id("docs", repo_scope_key()),
                source_name="docs",
                scope_key=repo_scope_key(),
                repo_commit_hash="abc123",
                cursor_text=None,
                watermark_text=None,
                watermark_timestamp=None,
                schema_version=None,
                last_attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                last_succeeded_at=datetime(2026, 3, 29, 10, 1, tzinfo=timezone.utc),
                last_error=None,
                metadata={"repo": "DripDropz/daedalus"},
            )
        ])

        with self.assertRaisesRegex(sync.SyncCommandError, "existing successful code baseline"):
            sync.load_required_incremental_baselines(store)

    def test_compute_docs_delta_treats_rename_as_delete_plus_add(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nfirst\n")
            baseline = self._git_commit_all(workspace, "baseline")

            subprocess.run(
                ["git", "mv", "README.md", "AGENTS.md"],
                cwd=workspace,
                check=True,
                capture_output=True,
                text=True,
            )
            self._git_commit_all(workspace, "rename")

            delta = sync.compute_docs_delta(workspace, baseline)

        self.assertEqual(delta.changed_paths, ("AGENTS.md",))
        self.assertEqual(delta.deleted_paths, ("README.md",))

    @unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
    def test_compute_code_delta_filters_supported_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._write_file(workspace / "README.md", "# docs\n")
            baseline = self._git_commit_all(workspace, "baseline")

            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 2;\n")
            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 1;\n")
            self._write_file(workspace / "README.md", "# docs changed\n")

            delta = sync.compute_code_delta(workspace, baseline)

        self.assertEqual(delta.changed_paths, ("source/common/alpha.ts", "source/common/beta.ts"))
        self.assertEqual(delta.deleted_paths, ())

    def test_sync_docs_changed_updates_changed_paths_and_deletes_removed_rows(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nupdated\n")
            docs_store = docs.InMemoryDocsStore()
            docs_store.upsert_documents(
                [
                    docs.PreparedDocument(
                        id="docs:AGENTS.md#0",
                        source_domain="docs",
                        doc_kind="agent_instruction",
                        source_path="AGENTS.md",
                        title="Agents",
                        section_title=None,
                        subsection_title=None,
                        heading_path=[],
                        chunk_index=0,
                        content="# Agents\n\nold\n",
                        preview_text="# Agents old",
                        content_hash=docs.deterministic_content_hash("AGENTS.md", "# Agents\n\nold\n", chunk_index=0),
                        repo_commit_hash="baseline-commit",
                        source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                        embedding=[1.0] * 384,
                        metadata={},
                    )
                ]
            )
            docs_store.upsert_documents([
                docs.PreparedDocument(
                    id="docs:README.md#0",
                    source_domain="docs",
                    doc_kind="readme",
                    source_path="README.md",
                    title="README",
                    section_title=None,
                    subsection_title=None,
                    heading_path=[],
                    chunk_index=0,
                    content="old readme",
                    preview_text="old readme",
                    content_hash="hash",
                    repo_commit_hash="baseline-commit",
                    source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
            ])

            sync_store = InMemorySyncStateStore()
            baseline = self._seed_docs_baseline(sync_store, repo_commit_hash="baseline-commit")
            result = sync._sync_docs_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=baseline,
                delta=sync.LocalDelta(
                    changed_paths=("AGENTS.md",),
                    deleted_paths=("README.md",),
                    baseline_commit="baseline-commit",
                    head_commit="head-commit",
                ),
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["changed_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ("AGENTS.md",))
        self.assertEqual(result["skipped_paths"], ())
        self.assertEqual(result["deleted_paths"], ("README.md",))
        self.assertIn(("AGENTS.md", 0), docs_store.rows_by_key)
        self.assertNotIn(("README.md", 0), docs_store.rows_by_key)
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)]["repo_commit_hash"], "head-commit")

    def test_sync_docs_changed_skips_unchanged_candidate_without_embedding_or_rewrite(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nstable\n")
            docs_store = docs.InMemoryDocsStore()
            docs_store.upsert_documents(
                docs.prepare_documents(
                    workspace,
                    source_paths=["AGENTS.md"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash="baseline-commit",
                )
            )
            original_row = dict(docs_store.rows_by_key[("AGENTS.md", 0)])

            sync_store = InMemorySyncStateStore()
            baseline = self._seed_docs_baseline(sync_store, repo_commit_hash="baseline-commit")
            embedding_client = FakeEmbeddingClient()
            result = sync._sync_docs_changed(
                workspace,
                embedding_client=embedding_client,
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=baseline,
                delta=sync.LocalDelta(
                    changed_paths=("AGENTS.md",),
                    deleted_paths=(),
                    baseline_commit="baseline-commit",
                    head_commit="head-commit",
                ),
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["changed_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ())
        self.assertEqual(result["skipped_paths"], ("AGENTS.md",))
        self.assertEqual(result["processed_count"], 0)
        self.assertEqual(embedding_client.calls, [])
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)], original_row)
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)]["repo_commit_hash"], "baseline-commit")
        state = sync_store.get_sync_state("docs", repo_scope_key())
        self.assertEqual(state.repo_commit_hash, "head-commit")
        self.assertEqual(state.metadata["candidate_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["updated_paths"], [])
        self.assertEqual(state.metadata["skipped_paths"], ["AGENTS.md"])

    def test_sync_docs_changed_replaces_shrinking_chunked_doc_and_keeps_unique_path_metadata(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "AGENTS.md",
                "# Agents\n\nupdated intro\n",
            )
            docs_store = docs.InMemoryDocsStore()
            docs_store.upsert_documents(
                docs.prepare_documents(
                    workspace,
                    source_paths=["AGENTS.md"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash="baseline-commit",
                )
            )
            docs_store.upsert_documents([
                docs.PreparedDocument(
                    id="docs:AGENTS.md#1",
                    source_domain="docs",
                    doc_kind="agent_instruction",
                    source_path="AGENTS.md",
                    title="Agents",
                    section_title="Rules",
                    subsection_title=None,
                    heading_path=["Rules"],
                    chunk_index=1,
                    content="## Rules\nold body",
                    preview_text="old body",
                    content_hash="hash-1",
                    repo_commit_hash="baseline-commit",
                    source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
            ])

            sync_store = InMemorySyncStateStore()
            baseline = self._seed_docs_baseline(sync_store, repo_commit_hash="baseline-commit")
            attempted_at = datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc)
            result = sync._sync_docs_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=baseline,
                delta=sync.LocalDelta(
                    changed_paths=("AGENTS.md",),
                    deleted_paths=(),
                    baseline_commit="baseline-commit",
                    head_commit="head-commit",
                ),
                attempted_at=attempted_at,
            )

        self.assertEqual(result["changed_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ("AGENTS.md",))
        self.assertEqual(result["skipped_paths"], ())
        self.assertEqual(result["processed_count"], 1)
        self.assertEqual(sorted(docs_store.rows_by_key), [("AGENTS.md", 0)])
        state = sync_store.get_sync_state("docs", repo_scope_key())
        self.assertEqual(state.metadata["source_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["updated_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["skipped_paths"], [])
        self.assertEqual(state.metadata["processed_count"], 1)

    def test_sync_docs_explicit_replaces_shrinking_docs_and_reports_chunk_count(self):
        docs_store = docs.InMemoryDocsStore()
        sync_store = InMemorySyncStateStore()

        docs_store.upsert_documents([
            docs.PreparedDocument(
                id="docs:AGENTS.md#0",
                source_domain="docs",
                doc_kind="agent_instruction",
                source_path="AGENTS.md",
                title="Agents",
                section_title=None,
                subsection_title=None,
                heading_path=[],
                chunk_index=0,
                content="# Agents\n\nold intro",
                preview_text="old intro",
                content_hash="hash-0",
                repo_commit_hash="baseline-commit",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={},
            ),
            docs.PreparedDocument(
                id="docs:AGENTS.md#1",
                source_domain="docs",
                doc_kind="agent_instruction",
                source_path="AGENTS.md",
                title="Agents",
                section_title="Rules",
                subsection_title=None,
                heading_path=["Rules"],
                chunk_index=1,
                content="## Rules\nold rules",
                preview_text="old rules",
                content_hash="hash-1",
                repo_commit_hash="baseline-commit",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={},
            ),
            docs.PreparedDocument(
                id="docs:README.md#0",
                source_domain="docs",
                doc_kind="readme",
                source_path="README.md",
                title="README",
                section_title=None,
                subsection_title=None,
                heading_path=[],
                chunk_index=0,
                content="# README\n\nold readme",
                preview_text="old readme",
                content_hash="hash-2",
                repo_commit_hash="baseline-commit",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={},
            ),
        ])

        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nnew intro\n")

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
                with patch("agentic_kb.commands.sync.get_head_commit", return_value="head-commit"):
                    with patch("agentic_kb.commands.sync.discover_docs_source_paths", return_value=["AGENTS.md"]):
                        with patch("agentic_kb.commands.sync.PostgresDocsStore.from_database_url") as docs_store_factory:
                            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                                docs_store_factory.return_value.__enter__.return_value = docs_store
                                docs_store_factory.return_value.__exit__.return_value = False
                                sync_store_factory.return_value.__enter__.return_value = sync_store
                                sync_store_factory.return_value.__exit__.return_value = False
                                result = sync.sync_docs(
                                    workspace,
                                    config=AgenticConfig(
                                        database_url="postgresql://localhost/test",
                                        ollama_base_url="http://ollama:11434",
                                        ollama_embed_model="all-minilm",
                                        github_token="token",
                                    ),
                                )

        self.assertEqual(result["candidate_paths"], ("AGENTS.md",))
        self.assertEqual(result["source_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ("AGENTS.md",))
        self.assertEqual(result["skipped_paths"], ())
        self.assertEqual(result["deleted_paths"], ("README.md",))
        self.assertEqual(result["processed_count"], 1)
        self.assertEqual(sorted(docs_store.rows_by_key), [("AGENTS.md", 0)])
        state = sync_store.get_sync_state("docs", repo_scope_key())
        self.assertEqual(state.metadata["source_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["candidate_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["updated_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["skipped_paths"], [])
        self.assertEqual(state.metadata["processed_count"], 1)

    def test_sync_docs_explicit_skips_unchanged_docs_and_reports_skipped_paths(self):
        docs_store = docs.InMemoryDocsStore()
        sync_store = InMemorySyncStateStore()

        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nstable\n")
            docs_store.upsert_documents(
                docs.prepare_documents(
                    workspace,
                    source_paths=["AGENTS.md"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash="baseline-commit",
                )
            )
            original_row = dict(docs_store.rows_by_key[("AGENTS.md", 0)])
            embedding_client = FakeEmbeddingClient()

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=embedding_client):
                with patch("agentic_kb.commands.sync.get_head_commit", return_value="head-commit"):
                    with patch("agentic_kb.commands.sync.discover_docs_source_paths", return_value=["AGENTS.md"]):
                        with patch("agentic_kb.commands.sync.PostgresDocsStore.from_database_url") as docs_store_factory:
                            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                                docs_store_factory.return_value.__enter__.return_value = docs_store
                                docs_store_factory.return_value.__exit__.return_value = False
                                sync_store_factory.return_value.__enter__.return_value = sync_store
                                sync_store_factory.return_value.__exit__.return_value = False
                                result = sync.sync_docs(
                                    workspace,
                                    config=AgenticConfig(
                                        database_url="postgresql://localhost/test",
                                        ollama_base_url="http://ollama:11434",
                                        ollama_embed_model="all-minilm",
                                        github_token="token",
                                    ),
                                )

        self.assertEqual(result["candidate_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ())
        self.assertEqual(result["skipped_paths"], ("AGENTS.md",))
        self.assertEqual(result["deleted_paths"], ())
        self.assertEqual(result["processed_count"], 0)
        self.assertEqual(embedding_client.calls, [])
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)], original_row)
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)]["repo_commit_hash"], "baseline-commit")
        state = sync_store.get_sync_state("docs", repo_scope_key())
        self.assertEqual(state.repo_commit_hash, "head-commit")
        self.assertEqual(state.metadata["candidate_paths"], ["AGENTS.md"])
        self.assertEqual(state.metadata["updated_paths"], [])
        self.assertEqual(state.metadata["skipped_paths"], ["AGENTS.md"])

    def test_sync_github_explicit_initial_run_seeds_all_stream_rows(self):
        sync_store = InMemorySyncStateStore()
        github_result = self._fake_github_result(updated_since=None)

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresGithubStore.from_database_url") as github_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_github", return_value=github_result) as ingest_github:
                        sync_store_factory.return_value.__enter__.return_value = sync_store
                        sync_store_factory.return_value.__exit__.return_value = False
                        github_store_factory.return_value.__enter__.return_value = github.InMemoryGithubStore()
                        github_store_factory.return_value.__exit__.return_value = False
                        result = sync.sync_github(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url="postgresql://localhost/test",
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                        )

        self.assertEqual(result["mode"], "initial")
        self.assertIsNone(ingest_github.call_args.kwargs["bounds"].updated_since)
        for stream_name in sync.STREAM_ORDER:
            state = sync_store.get_sync_state("github", github_scope_key(stream_name))
            self.assertIsNotNone(state)
            self.assertIsNotNone(state.last_succeeded_at)
            self.assertIsNotNone(state.watermark_timestamp)

    def test_sync_github_incremental_run_uses_earliest_stored_watermark(self):
        sync_store = InMemorySyncStateStore()
        baselines = self._seed_all_required_baselines(sync_store)
        github_result = self._fake_github_result(
            updated_since=datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc)
        )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresGithubStore.from_database_url") as github_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_github", return_value=github_result) as ingest_github:
                        sync_store_factory.return_value.__enter__.return_value = sync_store
                        sync_store_factory.return_value.__exit__.return_value = False
                        github_store_factory.return_value.__enter__.return_value = github.InMemoryGithubStore()
                        github_store_factory.return_value.__exit__.return_value = False
                        result = sync.sync_github(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url="postgresql://localhost/test",
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                        )

        self.assertEqual(result["mode"], "incremental")
        self.assertEqual(
            ingest_github.call_args.kwargs["bounds"].updated_since,
            datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
        )
        self.assertEqual(result["upstream_since_streams"], sync.UPSTREAM_SINCE_GITHUB_STREAMS)
        self.assertEqual(result["client_filtered_streams"], sync.CLIENT_FILTERED_GITHUB_STREAMS)
        self.assertEqual(baselines["github"]["pulls"].watermark_timestamp, datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc))

    def test_sync_github_changed_success_persists_updated_stream_states(self):
        sync_store = InMemorySyncStateStore()
        baselines = self._seed_all_required_baselines(sync_store)
        github_result = self._fake_github_result(
            updated_since=datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc)
        )

        with patch("agentic_kb.commands.sync.ingest_github", return_value=github_result) as ingest_github:
            result = sync._sync_github_changed(
                config=AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
                embedding_client=FakeEmbeddingClient(),
                github_store=github.InMemoryGithubStore(),
                sync_store=sync_store,
                baselines=baselines,
                attempted_at=datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["mode"], "incremental")
        self.assertEqual(result["updated_since"], datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc))
        self.assertEqual(
            ingest_github.call_args.kwargs["bounds"].updated_since,
            datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
        )
        for stream_name, expected_watermark in {
            "issues": datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
            "pulls": datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
            "issue_comments": datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc),
            "review_comments": datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc),
        }.items():
            state = sync_store.get_sync_state("github", github_scope_key(stream_name))
            self.assertIsNotNone(state)
            self.assertEqual(state.watermark_timestamp, expected_watermark)
            self.assertIsNotNone(state.last_succeeded_at)

    def test_load_required_incremental_baselines_rejects_missing_github_watermark(self):
        sync_store = InMemorySyncStateStore()
        self._seed_all_required_baselines(sync_store, github_watermarks={"issues": None}, validate=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "github baseline with watermark for issues"):
            sync.load_required_incremental_baselines(sync_store)

    def test_load_required_incremental_baselines_rejects_missing_project_cursor(self):
        sync_store = InMemorySyncStateStore()
        self._seed_all_required_baselines(sync_store, project_cursor_text=None, validate=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "non-empty cursor_text"):
            sync.load_required_incremental_baselines(sync_store)

    def test_sync_project_reuses_stored_cursor_and_preserves_end_cursor_noop(self):
        sync_store = InMemorySyncStateStore()
        baseline = PreparedSyncState(
            id=deterministic_sync_state_id("project", project_scope_key()),
            source_name="project",
            scope_key=project_scope_key(),
            repo_commit_hash=None,
            cursor_text="cursor-seeded",
            watermark_text="2026-03-29T08:30:00Z",
            watermark_timestamp=datetime(2026, 3, 29, 8, 30, tzinfo=timezone.utc),
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"project_owner": "DripDropz", "project_number": 5},
        )
        sync_store.upsert_sync_states([baseline])

        def fake_ingest_project_items(**kwargs):
            self.assertEqual(kwargs["bounds"].after_cursor, "cursor-seeded")
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=0,
                hit_bound=False,
                final_cursor="cursor-seeded",
                latest_source_updated_at=None,
                rows_written=0,
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = sync_store
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project.InMemoryProjectItemsStore()
                        project_store_factory.return_value.__exit__.return_value = False
                        result = sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url="postgresql://localhost/test",
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                        )

        self.assertEqual(result["mode"], "incremental")
        self.assertEqual(result["starting_cursor"], "cursor-seeded")
        self.assertEqual(result["final_cursor"], "cursor-seeded")
        self.assertEqual(result["pages_fetched"], 0)

    def test_sync_all_runs_expected_sources_in_order_and_stops_on_first_failure(self):
        calls = []

        def docs_handler(*args, **kwargs):
            calls.append("docs")
            return {"processed_count": 1, "deleted_paths": (), "repo_commit_hash": "a"}

        def github_handler(*args, **kwargs):
            calls.append("github")
            raise sync.SyncCommandError("github failed")

        with patch("agentic_kb.commands.sync.sync_docs", side_effect=docs_handler):
            with patch("agentic_kb.commands.sync.sync_github", side_effect=github_handler):
                with patch("agentic_kb.commands.sync.sync_code") as sync_code:
                    with patch("agentic_kb.commands.sync.sync_project") as sync_project:
                        with self.assertRaises(sync.SyncAllFailure) as error:
                            sync.sync_all(Path.cwd(), config=AgenticConfig(None, "http://ollama", "model", "token"))

        self.assertEqual(calls, ["docs", "github"])
        self.assertEqual(error.exception.source_name, "github")
        self.assertIn("docs", error.exception.partial_results)
        sync_code.assert_not_called()
        sync_project.assert_not_called()

    def test_sync_all_stage_start_callback_runs_in_new_order(self):
        calls = []
        stage_starts = []

        def docs_handler(*args, **kwargs):
            calls.append("docs")
            return {"processed_count": 1, "deleted_paths": (), "repo_commit_hash": "docs-sha", "mode": "explicit", "updated_paths": (), "source_paths": (), "skipped_paths": ()}

        def github_handler(*args, **kwargs):
            calls.append("github")
            return {
                "mode": "explicit",
                "updated_since": None,
                "stream_progress": {
                    stream_name: type("Progress", (), {"pages_fetched": 0, "hit_bound": False, "latest_source_updated_at": None})()
                    for stream_name in sync.STREAM_ORDER
                },
            }

        def project_handler(*args, **kwargs):
            calls.append("project")
            return {"mode": "explicit", "starting_cursor": None, "final_cursor": None, "pages_fetched": 0, "rows_written": 0}

        def code_handler(*args, **kwargs):
            calls.append("code")
            return {"mode": "explicit", "processed_file_count": 1, "chunk_count": 1, "repo_commit_hash": "code-sha"}

        with patch("agentic_kb.commands.sync.sync_docs", side_effect=docs_handler):
            with patch("agentic_kb.commands.sync.sync_github", side_effect=github_handler):
                with patch("agentic_kb.commands.sync.sync_project", side_effect=project_handler):
                    with patch("agentic_kb.commands.sync.sync_code", side_effect=code_handler):
                        sync.sync_all(
                            Path.cwd(),
                            config=AgenticConfig(None, "http://ollama", "model", "token"),
                            stage_start_callback=stage_starts.append,
                        )

        self.assertEqual(stage_starts, ["docs", "github", "project", "code"])
        self.assertEqual(calls, ["docs", "github", "project", "code"])

    def test_run_sync_all_prints_stage_start_lines_before_completion_summaries(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        result = {
            "docs": {
                "mode": "explicit",
                "updated_paths": (),
                "source_paths": (),
                "skipped_paths": (),
                "processed_count": 0,
                "deleted_paths": (),
                "repo_commit_hash": "docs-sha",
            },
            "github": {
                "mode": "explicit",
                "updated_since": None,
                "stream_progress": {
                    stream_name: type("Progress", (), {"pages_fetched": 0, "hit_bound": False, "latest_source_updated_at": None})()
                    for stream_name in sync.STREAM_ORDER
                },
            },
            "project": {
                "mode": "explicit",
                "starting_cursor": None,
                "final_cursor": None,
                "pages_fetched": 0,
                "rows_written": 0,
            },
            "code": {
                "mode": "explicit",
                "processed_file_count": 2,
                "chunk_count": 5,
                "repo_commit_hash": "code-sha",
            },
        }

        def sync_all_side_effect(*args, **kwargs):
            stage_start_callback = kwargs.get("stage_start_callback")
            for source_name in sync.SYNC_ALL_ORDER:
                stage_start_callback(source_name)
            return result

        with patch("agentic_kb.commands.sync.sync_all", side_effect=sync_all_side_effect):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = sync.run_sync_all(Namespace())

        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        output = stdout.getvalue()
        self.assertIn("starting sync docs...", output)
        self.assertIn("starting sync github...", output)
        self.assertIn("starting sync project...", output)
        self.assertIn("starting sync code...", output)
        self.assertLess(output.index("starting sync docs..."), output.index("sync docs completed"))
        self.assertLess(output.index("starting sync github..."), output.index("sync github completed"))
        self.assertLess(output.index("starting sync project..."), output.index("sync project completed"))
        self.assertLess(output.index("starting sync code..."), output.index("sync code completed"))

    def test_run_sync_changed_writes_errors_to_stderr_only(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch("agentic_kb.commands.sync.sync_changed", side_effect=sync.SyncCommandError("missing baseline")):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = sync.run_sync_changed(Namespace())

        self.assertEqual(exit_code, 1)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("sync changed failed: missing baseline", stderr.getvalue())

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_allows_local_only_kb_without_imported_snapshot_metadata(self, load_manifest):
        load_manifest.return_value = None

        sync.ensure_sync_changed_snapshot_compatibility(
            "postgresql://localhost/test",
            AgenticConfig(
                database_url="postgresql://localhost/test",
                ollama_base_url="http://ollama:11434",
                ollama_embed_model="all-minilm",
                github_token="token",
            ),
        )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_legacy_imported_manifest(self, load_manifest):
        load_manifest.return_value = _snapshot_record(None, legacy=True)

        with self.assertRaisesRegex(sync.SyncCommandError, "legacy embedding_model-only manifests are unsupported"):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_incompatible_imported_manifest_before_sync_work(self, load_manifest):
        load_manifest.return_value = _snapshot_record(
            {
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "different-model",
                "embedding_dimension": 384,
            }
        )

        with self.assertRaisesRegex(sync.SyncCommandError, "embedding model"):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_imported_manifest_with_extra_embedding_contract_keys(self, load_manifest):
        load_manifest.return_value = _snapshot_record(
            {
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "all-minilm",
                "embedding_dimension": 384,
                "unexpected": True,
            }
        )

        with self.assertRaisesRegex(sync.SyncCommandError, "unexpected fields"):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    @patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config")
    @patch("agentic_kb.commands.sync.ensure_sync_changed_snapshot_compatibility")
    @patch("agentic_kb.commands.sync._require_code_ingest_dependencies")
    def test_sync_changed_checks_snapshot_compatibility_before_creating_embedding_client(
        self,
        require_code_ingest_dependencies,
        ensure_compatibility,
        embedding_client_from_config,
    ):
        ensure_compatibility.side_effect = sync.SyncCommandError("contract mismatch")

        with self.assertRaisesRegex(sync.SyncCommandError, "contract mismatch"):
            sync.sync_changed(
                Path.cwd(),
                config=AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

        require_code_ingest_dependencies.assert_called_once_with("sync changed")
        embedding_client_from_config.assert_not_called()

    def test_sync_project_parser_accepts_full_flag(self):
        parser = cli.build_parser()
        args = parser.parse_args(["sync", "project", "--full"])
        self.assertTrue(args.full)
        self.assertIs(args.handler, sync.run_sync_project)

    def test_sync_project_parser_defaults_full_to_false(self):
        parser = cli.build_parser()
        args = parser.parse_args(["sync", "project"])
        self.assertFalse(args.full)

    def test_sync_changed_parser_rejects_full_flag(self):
        parser = cli.build_parser()
        with self.assertRaises(SystemExit):
            parser.parse_args(["sync", "changed", "--full"])

    def test_derive_project_cursor_for_explicit_sync_full_refresh(self):
        cursor, mode = sync.derive_project_cursor_for_explicit_sync(None, full_refresh=True)
        self.assertIsNone(cursor)
        self.assertEqual(mode, "full")

    def test_derive_project_cursor_for_explicit_sync_incremental_with_baseline(self):
        baseline = PreparedSyncState(
            id=deterministic_sync_state_id("project", project_scope_key()),
            source_name="project",
            scope_key=project_scope_key(),
            repo_commit_hash=None,
            cursor_text="stored-cursor",
            watermark_text="2026-03-29T08:30:00Z",
            watermark_timestamp=datetime(2026, 3, 29, 8, 30, tzinfo=timezone.utc),
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"project_owner": "DripDropz", "project_number": 5},
        )
        cursor, mode = sync.derive_project_cursor_for_explicit_sync(baseline, full_refresh=False)
        self.assertEqual(cursor, "stored-cursor")
        self.assertEqual(mode, "incremental")

    def test_sync_project_with_full_refresh_produces_mode_full(self):
        sync_store = InMemorySyncStateStore()
        baseline = PreparedSyncState(
            id=deterministic_sync_state_id("project", project_scope_key()),
            source_name="project",
            scope_key=project_scope_key(),
            repo_commit_hash=None,
            cursor_text="stored-cursor",
            watermark_text="2026-03-29T08:30:00Z",
            watermark_timestamp=datetime(2026, 3, 29, 8, 30, tzinfo=timezone.utc),
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"project_owner": "DripDropz", "project_number": 5},
        )
        sync_store.upsert_sync_states([baseline])

        def fake_ingest_project_items(**kwargs):
            self.assertIsNone(kwargs["bounds"].after_cursor)
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=3,
                hit_bound=True,
                final_cursor="new-end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=10,
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = sync_store
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project.InMemoryProjectItemsStore()
                        project_store_factory.return_value.__exit__.return_value = False
                        result = sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url="postgresql://localhost/test",
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                            full_refresh=True,
                        )

        self.assertEqual(result["mode"], "full")
        self.assertIsNone(result["starting_cursor"])
        self.assertEqual(result["final_cursor"], "new-end-cursor")
        self.assertEqual(result["pages_fetched"], 3)

    def test_format_sync_source_output_project_full_mode(self):
        result = {
            "mode": "full",
            "starting_cursor": None,
            "final_cursor": "end-cursor",
            "pages_fetched": 5,
            "rows_written": 20,
        }
        lines = sync.format_sync_source_output("project", result)
        self.assertEqual(lines[0], "sync project completed")
        self.assertIn("mode=full", lines[1])
        self.assertIn("note: Project sync ran in full-refresh mode and re-ingested all items.", lines)

    def test_format_sync_source_output_project_incremental_mode(self):
        result = {
            "mode": "incremental",
            "starting_cursor": "start-cursor",
            "final_cursor": "end-cursor",
            "pages_fetched": 1,
            "rows_written": 2,
        }
        lines = sync.format_sync_source_output("project", result)
        self.assertEqual(lines[0], "sync project completed")
        self.assertIn("mode=incremental", lines[1])
        self.assertIn("note: Project sync is cursor continuation only and can still miss edits to already-seen items.", lines)

    def test_format_sync_source_output_project_initial_mode(self):
        result = {
            "mode": "initial",
            "starting_cursor": None,
            "final_cursor": "end-cursor",
            "pages_fetched": 3,
            "rows_written": 15,
        }
        lines = sync.format_sync_source_output("project", result)
        self.assertEqual(lines[0], "sync project completed")
        self.assertIn("mode=initial", lines[1])
        self.assertEqual(len(lines), 2)

    def test_run_sync_project_passes_full_refresh_flag(self):
        captured_args = {}

        def capture_sync_project(root, *, config, full_refresh=False):
            captured_args["full_refresh"] = full_refresh
            return {
                "mode": "full" if full_refresh else "incremental",
                "starting_cursor": None,
                "final_cursor": "cursor",
                "pages_fetched": 0,
                "rows_written": 0,
            }

        with patch.object(sync, "sync_project", side_effect=capture_sync_project) as mock_sync:
            args = Namespace(full=True)
            sync.run_sync_project(args)

        self.assertTrue(captured_args["full_refresh"])
        mock_sync.assert_called_once()

    def _seed_docs_baseline(self, store: InMemorySyncStateStore, *, repo_commit_hash: str):
        state = PreparedSyncState(
            id=deterministic_sync_state_id("docs", repo_scope_key()),
            source_name="docs",
            scope_key=repo_scope_key(),
            repo_commit_hash=repo_commit_hash,
            cursor_text=None,
            watermark_text=None,
            watermark_timestamp=None,
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 9, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"repo": "DripDropz/daedalus"},
        )
        store.upsert_sync_states([state])
        return store.get_sync_state("docs", repo_scope_key())

    def _seed_all_required_baselines(
        self,
        store: InMemorySyncStateStore,
        *,
        github_watermarks=None,
        project_cursor_text="cursor-seeded",
        validate=True,
    ):
        github_watermarks = github_watermarks or {}
        docs_state = PreparedSyncState(
            id=deterministic_sync_state_id("docs", repo_scope_key()),
            source_name="docs",
            scope_key=repo_scope_key(),
            repo_commit_hash="docs-baseline",
            cursor_text=None,
            watermark_text=None,
            watermark_timestamp=None,
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"repo": "DripDropz/daedalus"},
        )
        code_state = PreparedSyncState(
            id=deterministic_sync_state_id("code", repo_scope_key()),
            source_name="code",
            scope_key=repo_scope_key(),
            repo_commit_hash="code-baseline",
            cursor_text=None,
            watermark_text=None,
            watermark_timestamp=None,
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"repo": "DripDropz/daedalus"},
        )
        defaults = {
            "issues": datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
            "pulls": datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            "issue_comments": datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            "review_comments": datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        }
        github_states = []
        for stream_name in sync.GITHUB_STREAM_NAMES:
            watermark_timestamp = github_watermarks.get(stream_name, defaults[stream_name])
            github_states.append(
                PreparedSyncState(
                    id=deterministic_sync_state_id("github", github_scope_key(stream_name)),
                    source_name="github",
                    scope_key=github_scope_key(stream_name),
                    repo_commit_hash=None,
                    cursor_text=None,
                    watermark_text=(
                        watermark_timestamp.isoformat().replace("+00:00", "Z")
                        if watermark_timestamp is not None
                        else None
                    ),
                    watermark_timestamp=watermark_timestamp,
                    schema_version=None,
                    last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"repo": "DripDropz/daedalus", "stream_name": stream_name},
                )
            )
        project_state = PreparedSyncState(
            id=deterministic_sync_state_id("project", project_scope_key()),
            source_name="project",
            scope_key=project_scope_key(),
            repo_commit_hash=None,
            cursor_text=project_cursor_text,
            watermark_text="2026-03-29T08:30:00Z",
            watermark_timestamp=datetime(2026, 3, 29, 8, 30, tzinfo=timezone.utc),
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
            last_error=None,
            metadata={"project_owner": "DripDropz", "project_number": 5},
        )
        store.upsert_sync_states([docs_state, code_state, *github_states, project_state])
        if not validate:
            return None
        return sync.load_required_incremental_baselines(store)

    def _fake_github_result(self, *, updated_since):
        return github.GithubIngestResult(
            repo="DripDropz/daedalus",
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", updated_since=updated_since),
            stream_progress={
                stream_name: github.GithubStreamProgress(
                    stream_name=stream_name,
                    pages_fetched=1 if stream_name in {"issues", "issue_comments"} else 0,
                    hit_bound=False,
                    latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc)
                    if stream_name in {"issues", "pulls"}
                    else datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc),
                    issues_written=1 if stream_name == "issues" else 0,
                    issue_comments_written=1 if stream_name == "issue_comments" else 0,
                    prs_written=1 if stream_name == "pulls" else 0,
                    pr_comments_written=1 if stream_name == "review_comments" else 0,
                )
                for stream_name in sync.STREAM_ORDER
            },
            issues_written=1,
            issue_comments_written=1,
            prs_written=1,
            pr_comments_written=1,
        )

    def _git_init(self, workspace: Path) -> None:
        subprocess.run(["git", "init"], cwd=workspace, check=True, capture_output=True, text=True)
        subprocess.run(
            ["git", "checkout", "-b", "main"],
            cwd=workspace,
            check=True,
            capture_output=True,
            text=True,
        )

    def _git_commit_all(self, workspace: Path, message: str) -> str:
        subprocess.run(["git", "add", "."], cwd=workspace, check=True, capture_output=True, text=True)
        subprocess.run(
            [
                "git",
                "-c",
                "user.name=Agent",
                "-c",
                "user.email=agent@example.com",
                "commit",
                "-m",
                message,
            ],
            cwd=workspace,
            check=True,
            capture_output=True,
            text=True,
        )
        completed = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=workspace,
            check=True,
            capture_output=True,
            text=True,
        )
        return completed.stdout.strip()

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


if __name__ == "__main__":
    unittest.main()


def _snapshot_record(contract, *, legacy=False):
    manifest = {
        "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
        "schema_version": 1,
        "snapshot_name": "imported-snapshot",
        "snapshot_created_at": "2026-03-29T12:00:00Z",
        "artifact": {
            "filename": "imported-snapshot.dump",
            "dump_format": "postgresql_custom",
            "compression": {"algorithm": "gzip", "level": 6},
            "size_bytes": 1,
            "content_hash": "sha256:" + "1" * 64,
        },
        "repo": {"name": "DripDropz/daedalus", "docs_commit_hash": None, "code_commit_hash": None},
        "entity_counts": {
            "documents": 0,
            "code_chunks": 0,
            "github_issues": 0,
            "github_issue_comments": 0,
            "github_prs": 0,
            "github_pr_comments": 0,
            "project_items": 0,
        },
        "sync_state": {
            "docs": {"repo_commit_hash": None, "last_synced_at": None},
            "code": {"repo_commit_hash": None, "last_synced_at": None},
            "github": {
                "issues": {"updated_at_watermark": None},
                "pulls": {"updated_at_watermark": None},
                "issue_comments": {"updated_at_watermark": None},
                "review_comments": {"updated_at_watermark": None},
            },
            "project": {"owner": "DripDropz", "number": 5, "cursor": None, "updated_at_watermark": None},
        },
    }
    if legacy:
        manifest["embedding_model"] = "all-minilm"
    else:
        manifest["embedding_contract"] = contract
    return SnapshotManifestRecord(
        snapshot_name="imported-snapshot",
        snapshot_created_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
        imported_at=datetime(2026, 3, 29, 12, 5, tzinfo=timezone.utc),
        source_path="/tmp/imported-snapshot.manifest.json",
        content_hash="sha256:" + "1" * 64,
        manifest=manifest,
    )
