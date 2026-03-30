from __future__ import annotations

import subprocess
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

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


class IncrementalSyncSmokeTests(unittest.TestCase):
    def test_docs_incremental_updates_only_changed_paths_not_all_docs(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nstable\n")
            self._write_file(workspace / "README.md", "# README\n\nstable\n")
            baseline = self._git_commit_all(workspace, "baseline")

            self._write_file(workspace / "AGENTS.md", "# Agents\n\nupdated\n")
            self._git_commit_all(workspace, "head")

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
                    content="# Agents\n\nstable\n",
                    preview_text="# Agents stable",
                    content_hash=docs.deterministic_content_hash("AGENTS.md", "# Agents\n\nstable\n", chunk_index=0),
                    repo_commit_hash=baseline,
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
                    content="# README\n\nstable\n",
                    preview_text="# README stable",
                    content_hash=docs.deterministic_content_hash("README.md", "# README\n\nstable\n", chunk_index=0),
                    repo_commit_hash=baseline,
                    source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                ),
            ])
            total_entity_count = len(docs_store.rows_by_key)
            self.assertEqual(total_entity_count, 2)

            sync_store = InMemorySyncStateStore()
            self._seed_docs_baseline(sync_store, repo_commit_hash=baseline)
            delta = sync.compute_docs_delta(workspace, baseline)

            result = sync._sync_docs_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=sync_store.get_sync_state("docs", repo_scope_key()),
                delta=delta,
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["changed_paths"], ("AGENTS.md",))
        self.assertEqual(result["updated_paths"], ("AGENTS.md",))
        self.assertIn(("AGENTS.md", 0), docs_store.rows_by_key)
        self.assertIn(("README.md", 0), docs_store.rows_by_key)
        self.assertEqual(len(result["updated_paths"]), 1)
        self.assertLess(len(result["updated_paths"]), total_entity_count)

    def test_docs_incremental_reports_deleted_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nold\n")
            baseline = self._git_commit_all(workspace, "baseline")

            subprocess.run(["git", "rm", "AGENTS.md"], cwd=workspace, check=True, capture_output=True, text=True)
            self._git_commit_all(workspace, "delete")

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
                    content_hash=docs.deterministic_content_hash("AGENTS.md", "# Agents\n\nold\n", chunk_index=0),
                    repo_commit_hash=baseline,
                    source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
            ])
            self.assertIn(("AGENTS.md", 0), docs_store.rows_by_key)

            sync_store = InMemorySyncStateStore()
            self._seed_docs_baseline(sync_store, repo_commit_hash=baseline)
            delta = sync.compute_docs_delta(workspace, baseline)

            result = sync._sync_docs_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=sync_store.get_sync_state("docs", repo_scope_key()),
                delta=delta,
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["deleted_paths"], ("AGENTS.md",))
        self.assertNotIn(("AGENTS.md", 0), docs_store.rows_by_key)

    def test_docs_incremental_treats_rename_as_delete_plus_add(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nold\n")
            baseline = self._git_commit_all(workspace, "baseline")

            subprocess.run(
                ["git", "mv", "README.md", "AGENTS.md"],
                cwd=workspace,
                check=True,
                capture_output=True,
                text=True,
            )
            self._git_commit_all(workspace, "rename")

            docs_store = docs.InMemoryDocsStore()
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
                    content="# Readme\n\nold\n",
                    preview_text="# Readme old",
                    content_hash=docs.deterministic_content_hash("README.md", "# Readme\n\nold\n", chunk_index=0),
                    repo_commit_hash=baseline,
                    source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
            ])
            self.assertIn(("README.md", 0), docs_store.rows_by_key)
            self.assertNotIn(("AGENTS.md", 0), docs_store.rows_by_key)

            sync_store = InMemorySyncStateStore()
            self._seed_docs_baseline(sync_store, repo_commit_hash=baseline)
            delta = sync.compute_docs_delta(workspace, baseline)

            result = sync._sync_docs_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=sync_store.get_sync_state("docs", repo_scope_key()),
                delta=delta,
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(set(result["deleted_paths"]), {"README.md"})
        self.assertEqual(set(result["changed_paths"]), {"AGENTS.md"})
        self.assertNotIn(("README.md", 0), docs_store.rows_by_key)
        self.assertIn(("AGENTS.md", 0), docs_store.rows_by_key)

    @unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
    def test_code_incremental_updates_only_changed_paths_not_all_code(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._write_file(workspace / "source/common/beta.ts", "export const Beta = 1;\n")
            baseline = self._git_commit_all(workspace, "baseline")

            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 2;\n")
            self._git_commit_all(workspace, "head")

            code_store = code.InMemoryCodeChunksStore()
            code_store.upsert_chunks(
                code.prepare_code_chunks(
                    workspace,
                    source_paths=["source/common/alpha.ts", "source/common/beta.ts"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash=baseline,
                )
            )
            total_entity_count = len(code_store.rows_by_key)
            self.assertGreater(total_entity_count, 1)

            sync_store = InMemorySyncStateStore()
            self._seed_code_baseline(sync_store, repo_commit_hash=baseline)
            delta = sync.compute_code_delta(workspace, baseline)

            result = sync._sync_code_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                code_store=code_store,
                sync_store=sync_store,
                baseline=sync_store.get_sync_state("code", repo_scope_key()),
                delta=delta,
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["changed_paths"], ("source/common/alpha.ts",))
        self.assertIn("source/common/alpha.ts", result["changed_paths"])
        self.assertNotIn("source/common/beta.ts", result["changed_paths"])
        self.assertEqual(result["processed_file_count"], 1)
        self.assertLess(result["processed_file_count"], 2)

    @unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
    def test_code_incremental_reports_deleted_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline = self._git_commit_all(workspace, "baseline")

            subprocess.run(["git", "rm", "source/common/alpha.ts"], cwd=workspace, check=True, capture_output=True, text=True)
            self._git_commit_all(workspace, "delete")

            code_store = code.InMemoryCodeChunksStore()
            code_store.upsert_chunks(
                code.prepare_code_chunks(
                    workspace,
                    source_paths=["source/common/alpha.ts"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash=baseline,
                )
            )
            initial_keys = set(code_store.rows_by_key.keys())
            self.assertTrue(any("alpha" in str(k) for k in initial_keys))

            sync_store = InMemorySyncStateStore()
            self._seed_code_baseline(sync_store, repo_commit_hash=baseline)
            delta = sync.compute_code_delta(workspace, baseline)

            result = sync._sync_code_changed(
                workspace,
                embedding_client=FakeEmbeddingClient(),
                code_store=code_store,
                sync_store=sync_store,
                baseline=sync_store.get_sync_state("code", repo_scope_key()),
                delta=delta,
                attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["deleted_paths"], ("source/common/alpha.ts",))
        self.assertEqual(len(result["changed_paths"]), 0)

    def test_github_incremental_uses_minimum_watermark_as_updated_since(self):
        sync_store = InMemorySyncStateStore()
        baselines = self._seed_all_required_baselines(sync_store)
        github_store = github.InMemoryGithubStore()

        github_result = self._fake_github_result(
            updated_since=datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
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
                github_store=github_store,
                sync_store=sync_store,
                baselines=baselines,
                attempted_at=datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(result["mode"], "incremental")
        self.assertEqual(
            ingest_github.call_args.kwargs["bounds"].updated_since,
            datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
        )

    def test_load_required_incremental_baselines_fails_without_docs_baseline(self):
        store = InMemorySyncStateStore()
        self._seed_incomplete_baselines(store, include_docs=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "existing successful docs baseline"):
            sync.load_required_incremental_baselines(store)

    def test_load_required_incremental_baselines_fails_without_code_baseline(self):
        store = InMemorySyncStateStore()
        self._seed_incomplete_baselines(store, include_code=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "existing successful code baseline"):
            sync.load_required_incremental_baselines(store)

    def test_load_required_incremental_baselines_fails_without_project_baseline(self):
        store = InMemorySyncStateStore()
        self._seed_incomplete_baselines(store, include_project=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "existing successful project baseline"):
            sync.load_required_incremental_baselines(store)

    def test_load_required_incremental_baselines_fails_without_github_watermark(self):
        store = InMemorySyncStateStore()
        self._seed_incomplete_baselines(store, github_watermarks={"issues": None})

        with self.assertRaisesRegex(sync.SyncCommandError, "github baseline with watermark"):
            sync.load_required_incremental_baselines(store)

    def test_ensure_sync_changed_snapshot_compatibility_rejects_legacy_manifest(self):
        load_manifest = self._make_snapshot_record(legacy=True)

        with patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest", return_value=load_manifest):
            with self.assertRaisesRegex(sync.SyncCommandError, "legacy embedding_model-only manifests"):
                sync.ensure_sync_changed_snapshot_compatibility(
                    "postgresql://localhost/test",
                    AgenticConfig(
                        database_url="postgresql://localhost/test",
                        ollama_base_url="http://ollama:11434",
                        ollama_embed_model="all-minilm",
                        github_token="token",
                    ),
                )

    def test_ensure_sync_changed_snapshot_compatibility_rejects_incompatible_model(self):
        load_manifest = self._make_snapshot_record(
            contract={
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "different-model",
                "embedding_dimension": 384,
            }
        )

        with patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest", return_value=load_manifest):
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

    def test_ensure_sync_changed_snapshot_compatibility_accepts_compatible_manifest(self):
        load_manifest = self._make_snapshot_record(
            contract={
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "all-minilm",
                "embedding_dimension": 384,
            }
        )

        with patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest", return_value=load_manifest):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    def test_ensure_sync_changed_snapshot_compatibility_allows_no_manifest(self):
        with patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest", return_value=None):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

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

    def _seed_code_baseline(self, store: InMemorySyncStateStore, *, repo_commit_hash: str):
        state = PreparedSyncState(
            id=deterministic_sync_state_id("code", repo_scope_key()),
            source_name="code",
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
        return store.get_sync_state("code", repo_scope_key())

    def _seed_all_required_baselines(self, store: InMemorySyncStateStore):
        states = []

        states.append(
            PreparedSyncState(
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
        )

        states.append(
            PreparedSyncState(
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
        )

        defaults = {
            "issues": datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
            "pulls": datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            "issue_comments": datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            "review_comments": datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        }
        for stream_name in sync.GITHUB_STREAM_NAMES:
            watermark_timestamp = defaults[stream_name]
            states.append(
                PreparedSyncState(
                    id=deterministic_sync_state_id("github", github_scope_key(stream_name)),
                    source_name="github",
                    scope_key=github_scope_key(stream_name),
                    repo_commit_hash=None,
                    cursor_text=None,
                    watermark_text=watermark_timestamp.isoformat().replace("+00:00", "Z"),
                    watermark_timestamp=watermark_timestamp,
                    schema_version=None,
                    last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"repo": "DripDropz/daedalus", "stream_name": stream_name},
                )
            )

        states.append(
            PreparedSyncState(
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
        )

        store.upsert_sync_states(states)
        return sync.load_required_incremental_baselines(store)

    def _seed_incomplete_baselines(
        self,
        store: InMemorySyncStateStore,
        *,
        github_watermarks=None,
        include_docs=True,
        include_code=True,
        include_project=True,
    ):
        github_watermarks = github_watermarks or {}
        states = []

        if include_docs:
            states.append(
                PreparedSyncState(
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
            )

        if include_code:
            states.append(
                PreparedSyncState(
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
            )

        defaults = {
            "issues": datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
            "pulls": datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            "issue_comments": datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            "review_comments": datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        }
        for stream_name in sync.GITHUB_STREAM_NAMES:
            watermark_timestamp = github_watermarks.get(stream_name, defaults[stream_name])
            states.append(
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

        if include_project:
            states.append(
                PreparedSyncState(
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
            )

        store.upsert_sync_states(states)

    def _fake_github_result(self, *, updated_since, new_issues=None, new_issue_comments=None, new_prs=None, new_pr_comments=None):
        new_issues = new_issues or []
        new_issue_comments = new_issue_comments or []
        new_prs = new_prs or []
        new_pr_comments = new_pr_comments or []
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
                    issues_written=len(new_issues) if stream_name == "issues" else 0,
                    issue_comments_written=len(new_issue_comments) if stream_name == "issue_comments" else 0,
                    prs_written=len(new_prs) if stream_name == "pulls" else 0,
                    pr_comments_written=len(new_pr_comments) if stream_name == "review_comments" else 0,
                )
                for stream_name in sync.STREAM_ORDER
            },
            issues_written=len(new_issues),
            issue_comments_written=len(new_issue_comments),
            prs_written=len(new_prs),
            pr_comments_written=len(new_pr_comments),
        )

    def _make_snapshot_record(self, contract=None, *, legacy=False):
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
