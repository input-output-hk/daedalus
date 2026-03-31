from __future__ import annotations

import os
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path
from unittest.mock import patch

from agentic_kb.commands import sync
from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import docs, project

try:
    from agentic_kb.ingest import code
except ImportError:  # pragma: no cover
    code = None

try:
    from agentic_kb.ingest.code import InMemoryCodeChunksStore
except ImportError:  # pragma: no cover
    InMemoryCodeChunksStore = None

from agentic_kb.sync.state import (
    InMemorySyncStateStore,
    PreparedSyncState,
    deterministic_sync_state_id,
    project_scope_key,
    repo_scope_key,
)


class FakeEmbeddingClient:
    def __init__(self):
        self.calls = []

    def embed_texts(self, texts):
        self.calls.append(list(texts))
        return [[1.0] * 384 for _ in texts]


def _make_project_item(
    node_id: str,
    title: str = "Item",
    body: str = "Body",
    is_archived: bool = False,
    status: str | None = None,
    priority: str | None = None,
    size: str | None = None,
    work_type: str | None = None,
    area: str | None = None,
    phase: str | None = None,
    kb_impact: str | None = None,
    start_date=None,
    target_date=None,
):
    return project.PreparedProjectItem(
        id=f"item-{node_id}",
        project_owner="DripDropz",
        project_number=5,
        project_item_node_id=node_id,
        content_type="Issue",
        content_id=f"content-{node_id}",
        content_node_id=f"content-node-{node_id}",
        title=title,
        body_text=body,
        repo="DripDropz/daedalus",
        status=status,
        priority=priority,
        size=size,
        work_type=work_type,
        area=area,
        phase=phase,
        kb_impact=kb_impact,
        start_date=start_date,
        target_date=target_date,
        field_values={},
        html_url=f"https://github.com/issues/{node_id}",
        source_updated_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        embedding=[1.0] * 384,
        metadata={"is_archived": is_archived},
    )


@unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
class DocsCodeRemovalReconciliationTests(unittest.TestCase):
    def test_sync_docs_combined_add_and_delete_converges(self):
        docs_store = docs.InMemoryDocsStore()
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
                content="# Agents\n\nold\n",
                preview_text="# Agents old",
                content_hash="hash-agents-old",
                repo_commit_hash="baseline",
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
                content="# README\n\nold\n",
                preview_text="# README old",
                content_hash="hash-readme",
                repo_commit_hash="baseline",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={},
            ),
            docs.PreparedDocument(
                id="docs:CLAUDE.md#0",
                source_domain="docs",
                doc_kind="agent_instruction",
                source_path="CLAUDE.md",
                title="CLAUDE",
                section_title=None,
                subsection_title=None,
                heading_path=[],
                chunk_index=0,
                content="# Claude\n\nold\n",
                preview_text="# Claude old",
                content_hash="hash-claude",
                repo_commit_hash="baseline",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={},
            ),
        ])
        self.assertEqual(len(docs_store.rows_by_key), 3)

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.get_head_commit", return_value="head-commit"):
                with patch("agentic_kb.commands.sync.discover_docs_source_paths", return_value=["AGENTS.md"]):
                    with patch("agentic_kb.commands.sync.PostgresDocsStore.from_database_url") as docs_store_factory:
                        with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                            docs_store_factory.return_value.__enter__.return_value = docs_store
                            docs_store_factory.return_value.__exit__.return_value = False
                            sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                            sync_store_factory.return_value.__exit__.return_value = False
                            result = sync.sync_docs(
                                Path.cwd(),
                                config=AgenticConfig(
                                    database_url="postgresql://localhost/test",
                                    ollama_base_url="http://ollama:11434",
                                    ollama_embed_model="all-minilm",
                                    github_token="token",
                                ),
                            )

        self.assertEqual(result["deleted_paths"], ("CLAUDE.md", "README.md"))
        self.assertEqual(result["updated_paths"], ("AGENTS.md",))
        self.assertNotIn(("README.md", 0), docs_store.rows_by_key)
        self.assertNotIn(("CLAUDE.md", 0), docs_store.rows_by_key)
        agents_keys = [k for k in docs_store.rows_by_key if k[0] == "AGENTS.md"]
        self.assertTrue(len(agents_keys) >= 1)

    def test_sync_code_supported_path_filtering_removes_stale_chunks(self):
        code_store = InMemoryCodeChunksStore()
        alpha_chunk = code.PreparedCodeChunk(
            id="code:alpha.ts#0",
            repo_path="alpha.ts",
            language="typescript",
            symbol_name="Alpha",
            symbol_kind="variable",
            parent_symbol_name=None,
            parent_symbol_kind=None,
            chunk_index=0,
            start_line=1,
            end_line=1,
            content="export const Alpha = 1;",
            preview_text="export const Alpha = 1;",
            content_hash="hash-alpha",
            repo_commit_hash="baseline",
            embedding=[1.0] * 384,
            metadata={},
        )
        pipeline_chunk = code.PreparedCodeChunk(
            id="code:pipeline.yml#0",
            repo_path="pipeline.yml",
            language="yaml",
            symbol_name=None,
            symbol_kind="file_chunk",
            parent_symbol_name=None,
            parent_symbol_kind=None,
            chunk_index=0,
            start_line=1,
            end_line=1,
            content="steps:\n  - test",
            preview_text="steps: - test",
            content_hash="hash-pipeline",
            repo_commit_hash="baseline",
            embedding=[1.0] * 384,
            metadata={},
        )
        code_store.replace_chunks_for_path("alpha.ts", [alpha_chunk])
        code_store.replace_chunks_for_path("pipeline.yml", [pipeline_chunk])
        self.assertEqual(len(code_store.rows_by_key), 2)

        def fake_resolve_source_paths(*args, **kwargs):
            return ["alpha.ts"]

        def fake_prepare_chunks(workspace_root, repo_path, **kwargs):
            if repo_path == "alpha.ts":
                return [alpha_chunk]
            return []

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.get_head_commit", return_value="head-commit"):
                with patch("agentic_kb.ingest.code._resolve_source_paths", side_effect=fake_resolve_source_paths):
                    with patch("agentic_kb.ingest.code._prepare_file_code_chunks", side_effect=fake_prepare_chunks):
                        with patch("agentic_kb.commands.sync.PostgresCodeChunksStore.from_database_url") as code_store_factory:
                            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                                code_store_factory.return_value.__enter__.return_value = code_store
                                code_store_factory.return_value.__exit__.return_value = False
                                sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                                sync_store_factory.return_value.__exit__.return_value = False
                                result = sync.sync_code(
                                    Path.cwd(),
                                    config=AgenticConfig(
                                        database_url="postgresql://localhost/test",
                                        ollama_base_url="http://ollama:11434",
                                        ollama_embed_model="all-minilm",
                                        github_token="token",
                                    ),
                                )

        self.assertEqual(result["mode"], "explicit")
        self.assertEqual(len(code_store.rows_by_key), 1)
        self.assertTrue(all(k[0] == "alpha.ts" for k in code_store.rows_by_key))

    def test_sync_code_combined_add_and_delete_converges(self):
        code_store = InMemoryCodeChunksStore()
        alpha_chunk = code.PreparedCodeChunk(
            id="code:alpha.ts#0",
            repo_path="alpha.ts",
            language="typescript",
            symbol_name="Alpha",
            symbol_kind="variable",
            parent_symbol_name=None,
            parent_symbol_kind=None,
            chunk_index=0,
            start_line=1,
            end_line=1,
            content="export const Alpha = 1;",
            preview_text="export const Alpha = 1;",
            content_hash="hash-alpha",
            repo_commit_hash="baseline",
            embedding=[1.0] * 384,
            metadata={},
        )
        old_chunk = code.PreparedCodeChunk(
            id="code:old.ts#0",
            repo_path="old.ts",
            language="typescript",
            symbol_name="Old",
            symbol_kind="variable",
            parent_symbol_name=None,
            parent_symbol_kind=None,
            chunk_index=0,
            start_line=1,
            end_line=1,
            content="export const Old = 1;",
            preview_text="export const Old = 1;",
            content_hash="hash-old",
            repo_commit_hash="baseline",
            embedding=[1.0] * 384,
            metadata={},
        )
        code_store.replace_chunks_for_path("alpha.ts", [alpha_chunk])
        code_store.replace_chunks_for_path("old.ts", [old_chunk])
        self.assertEqual(len(code_store.rows_by_key), 2)

        def fake_resolve_source_paths(*args, **kwargs):
            return ["alpha.ts"]

        def fake_prepare_chunks(workspace_root, repo_path, **kwargs):
            if repo_path == "alpha.ts":
                return [alpha_chunk]
            return []

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.get_head_commit", return_value="head-commit"):
                with patch("agentic_kb.ingest.code._resolve_source_paths", side_effect=fake_resolve_source_paths):
                    with patch("agentic_kb.ingest.code._prepare_file_code_chunks", side_effect=fake_prepare_chunks):
                        with patch("agentic_kb.commands.sync.PostgresCodeChunksStore.from_database_url") as code_store_factory:
                            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                                code_store_factory.return_value.__enter__.return_value = code_store
                                code_store_factory.return_value.__exit__.return_value = False
                                sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                                sync_store_factory.return_value.__exit__.return_value = False
                                result = sync.sync_code(
                                    Path.cwd(),
                                    config=AgenticConfig(
                                        database_url="postgresql://localhost/test",
                                        ollama_base_url="http://ollama:11434",
                                        ollama_embed_model="all-minilm",
                                        github_token="token",
                                    ),
                                )

        self.assertEqual(result["mode"], "explicit")
        self.assertEqual(len(code_store.rows_by_key), 1)
        self.assertIn(("alpha.ts", 0), code_store.rows_by_key)
        self.assertNotIn(("old.ts", 0), code_store.rows_by_key)


class ProjectRemovalReconciliationTests(unittest.TestCase):
    def test_sync_project_full_multi_item_convergence(self):
        project_store = project.InMemoryProjectItemsStore()
        items = [
            _make_project_item("node-1", title="Live 1", is_archived=False),
            _make_project_item("node-2", title="Live 2", is_archived=False),
            _make_project_item("node-3", title="Live 3", is_archived=False),
            _make_project_item("node-archived", title="Archived", is_archived=True),
            _make_project_item("node-deleted", title="Deleted", is_archived=False),
        ]
        project_store.upsert_project_items(items)
        self.assertEqual(len(project_store.rows_by_key), 5)

        def fake_ingest_project_items(**kwargs):
            self.assertIsNone(kwargs["bounds"].after_cursor)
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=3,
                seen_node_ids=frozenset({"node-1", "node-2", "node-3"}),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project_store
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
        self.assertEqual(len(project_store.rows_by_key), 3)
        self.assertIn("node-1", project_store.rows_by_key)
        self.assertIn("node-2", project_store.rows_by_key)
        self.assertIn("node-3", project_store.rows_by_key)
        self.assertNotIn("node-archived", project_store.rows_by_key)
        self.assertNotIn("node-deleted", project_store.rows_by_key)

    def test_sync_project_full_preserves_cleared_field_values(self):
        project_store = project.InMemoryProjectItemsStore()
        item = _make_project_item(
            "node-1",
            title="Cleared Fields",
            status=None,
            priority=None,
            size=None,
        )
        project_store.upsert_project_items([item])
        self.assertIsNone(project_store.rows_by_key["node-1"]["status"])
        self.assertIsNone(project_store.rows_by_key["node-1"]["priority"])
        self.assertIsNone(project_store.rows_by_key["node-1"]["size"])

        def fake_ingest_project_items(**kwargs):
            self.assertIsNone(kwargs["bounds"].after_cursor)
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=1,
                seen_node_ids=frozenset({"node-1"}),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project_store
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
        self.assertEqual(len(project_store.rows_by_key), 1)
        self.assertIn("node-1", project_store.rows_by_key)

    def test_sync_project_incremental_noop_when_no_archived_items(self):
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

        project_store = project.InMemoryProjectItemsStore()
        items = [
            _make_project_item("node-1", title="Live 1", is_archived=False),
            _make_project_item("node-2", title="Live 2", is_archived=False),
        ]
        project_store.upsert_project_items(items)
        self.assertEqual(len(project_store.rows_by_key), 2)

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
                seen_node_ids=frozenset(),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = sync_store
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project_store
                        project_store_factory.return_value.__exit__.return_value = False
                        result = sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url="postgresql://localhost/test",
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                            full_refresh=False,
                        )

        self.assertEqual(result["mode"], "incremental")
        self.assertEqual(len(project_store.rows_by_key), 2)
        self.assertIn("node-1", project_store.rows_by_key)
        self.assertIn("node-2", project_store.rows_by_key)


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class RemovalReconciliationDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)

    def test_sync_project_full_removes_archived_items_from_db(self):
        self._seed_project_item("node-1", "Live 1", is_archived=False)
        self._seed_project_item("node-2", "Archived 1", is_archived=True)
        self._seed_project_item("node-3", "Archived 2", is_archived=True)

        def fake_ingest_project_items(**kwargs):
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=1,
                seen_node_ids=frozenset({"node-1"}),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project.PostgresProjectItemsStore(self.connection)
                        project_store_factory.return_value.__exit__.return_value = False
                        sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url=self.database_url,
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                            full_refresh=True,
                        )

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT COUNT(*) FROM agentic.kb_project_items WHERE metadata->>'is_archived' = 'true'"
            )
            archived_count = cursor.fetchone()[0]
            cursor.execute(
                "SELECT COUNT(*) FROM agentic.kb_project_items"
            )
            total_count = cursor.fetchone()[0]

        self.assertEqual(archived_count, 0)
        self.assertEqual(total_count, 1)

    def test_sync_project_full_removes_deleted_items_from_db(self):
        self._seed_project_item("node-1", "Live 1", is_archived=False)
        self._seed_project_item("node-2", "Live 2", is_archived=False)
        self._seed_project_item("node-3", "To Delete", is_archived=False)

        def fake_ingest_project_items(**kwargs):
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=2,
                seen_node_ids=frozenset({"node-1", "node-2"}),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project.PostgresProjectItemsStore(self.connection)
                        project_store_factory.return_value.__exit__.return_value = False
                        sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url=self.database_url,
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                            full_refresh=True,
                        )

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT COUNT(*) FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                ("node-3",),
            )
            deleted_count = cursor.fetchone()[0]
            cursor.execute(
                "SELECT COUNT(*) FROM agentic.kb_project_items"
            )
            total_count = cursor.fetchone()[0]

        self.assertEqual(deleted_count, 0)
        self.assertEqual(total_count, 2)

    def test_sync_project_preserves_cleared_field_values_in_db(self):
        self._seed_project_item_with_null_fields("node-1", "Cleared Fields")

        def fake_ingest_project_items(**kwargs):
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="end-cursor",
                latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                rows_written=1,
                seen_node_ids=frozenset({"node-1"}),
            )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.PostgresSyncStateStore.from_database_url") as sync_store_factory:
                with patch("agentic_kb.commands.sync.PostgresProjectItemsStore.from_database_url") as project_store_factory:
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        sync_store_factory.return_value.__enter__.return_value = InMemorySyncStateStore()
                        sync_store_factory.return_value.__exit__.return_value = False
                        project_store_factory.return_value.__enter__.return_value = project.PostgresProjectItemsStore(self.connection)
                        project_store_factory.return_value.__exit__.return_value = False
                        sync.sync_project(
                            Path.cwd(),
                            config=AgenticConfig(
                                database_url=self.database_url,
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                            full_refresh=True,
                        )

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT metadata->>'status', metadata->>'priority', metadata->>'size' "
                "FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                ("node-1",),
            )
            row = cursor.fetchone()

        self.assertIsNotNone(row)
        self.assertEqual(row[0], "false")
        self.assertEqual(row[1], "false")
        self.assertEqual(row[2], "false")

    def _seed_project_item(self, node_id, title, is_archived=False):
        import json
        with self.connection.cursor() as cursor:
            cursor.execute(
                """
                INSERT INTO agentic.kb_project_items (
                    id, project_owner, project_number, project_item_node_id,
                    content_type, content_id, content_node_id, title, body_text,
                    repo, status, priority, size, work_type, area, phase,
                    kb_impact, start_date, target_date, field_values,
                    html_url, source_updated_at, embedding, metadata, updated_at
                ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
                          %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, NOW())
                """,
                (
                    f"github-project-item:DripDropz/5:{node_id}",
                    "DripDropz", 5, node_id,
                    "Issue", f"content-{node_id}", f"content-node-{node_id}",
                    title, f"Body for {title}",
                    "DripDropz/daedalus", None, None, None, None, None, None,
                    None, None, None, json.dumps({}),
                    f"https://github.com/issues/{node_id}",
                    datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    "[0.0] * 384",
                    json.dumps({"is_archived": is_archived}),
                ),
            )

    def _seed_project_item_with_null_fields(self, node_id, title):
        import json
        with self.connection.cursor() as cursor:
            cursor.execute(
                """
                INSERT INTO agentic.kb_project_items (
                    id, project_owner, project_number, project_item_node_id,
                    content_type, content_id, content_node_id, title, body_text,
                    repo, status, priority, size, work_type, area, phase,
                    kb_impact, start_date, target_date, field_values,
                    html_url, source_updated_at, embedding, metadata, updated_at
                ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s,
                          %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, NOW())
                """,
                (
                    f"github-project-item:DripDropz/5:{node_id}",
                    "DripDropz", 5, node_id,
                    "Issue", f"content-{node_id}", f"content-node-{node_id}",
                    title, f"Body for {title}",
                    "DripDropz/daedalus", None, None, None, None, None, None,
                    None, None, None, json.dumps({}),
                    f"https://github.com/issues/{node_id}",
                    datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    "[0.0] * 384",
                    json.dumps({"is_archived": False}),
                ),
            )


def _bootstrap_database(connection):
    schema_dir = Path(__file__).resolve().parents[1] / "schema"
    init_sql = _sanitized_sql(schema_dir / "init.sql")
    create_indexes_sql = (schema_dir / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def _sanitized_sql(path):
    lines = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip().startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


if __name__ == "__main__":
    unittest.main()
