from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
import json
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path
from unittest.mock import patch

from agentic_kb.commands import sync
from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import docs, github, project
from agentic_kb.snapshot_manifest import SNAPSHOT_EMBEDDING_CONTRACT_ID
from agentic_kb.sync.state import (
    DEFAULT_PROJECT_NUMBER,
    DEFAULT_PROJECT_OWNER,
    DEFAULT_SYNC_REPO,
    GITHUB_STREAM_NAMES,
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


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


class FakeEmbeddingClient:
    def embed_texts(self, texts):
        return [[1.0] * 384 for _ in texts]


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None and code is not None,
    "requires AGENTIC_TEST_DATABASE_URL, psycopg, and optional tree-sitter dependencies",
)
class SyncCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)

    def test_sync_changed_reuses_all_stored_github_watermarks_and_project_cursor(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(
                workspace / "README.md",
                "# Readme\n\nintro\n\n## Section\nbaseline\n",
            )
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")

            self._seed_imported_baseline(workspace, baseline_commit)

            subprocess.run(
                ["git", "mv", "README.md", "AGENTS.md"],
                cwd=workspace,
                check=True,
                capture_output=True,
                text=True,
            )
            subprocess.run(
                ["git", "mv", "source/common/alpha.ts", "source/common/renamedAlpha.ts"],
                cwd=workspace,
                check=True,
                capture_output=True,
                text=True,
            )
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nchanged after import\n")
            self._write_file(workspace / "source/common/renamedAlpha.ts", "export const Alpha = 2;\n")
            head_commit = self._git_commit_all(workspace, "rename after import")

            github_bounds = {}
            project_bounds = {}

            def fake_ingest_github(**kwargs):
                github_bounds["bounds"] = kwargs["bounds"]
                return self._fake_github_result(kwargs["bounds"])

            def fake_ingest_project_items(**kwargs):
                project_bounds["bounds"] = kwargs["bounds"]
                return project.ProjectIngestResult(
                    project_owner=DEFAULT_PROJECT_OWNER,
                    project_number=DEFAULT_PROJECT_NUMBER,
                    project_title="Daedalus Maintenance",
                    project_url="https://github.com/orgs/DripDropz/projects/5",
                    bounds=kwargs["bounds"],
                    pages_fetched=1,
                    hit_bound=False,
                    final_cursor="cursor-next",
                    latest_source_updated_at=datetime(2026, 3, 29, 13, 0, tzinfo=timezone.utc),
                    rows_written=2,
                )

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
                with patch("agentic_kb.commands.sync.ingest_github", side_effect=fake_ingest_github):
                    with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                        result = sync.sync_changed(
                            workspace,
                            config=AgenticConfig(
                                database_url=self.database_url,
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                        )

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT source_path, chunk_index, content, repo_commit_hash FROM agentic.kb_documents ORDER BY source_path, chunk_index"
                )
                doc_rows = cursor.fetchall()
                cursor.execute(
                    "SELECT DISTINCT repo_path, repo_commit_hash FROM agentic.kb_code_chunks ORDER BY repo_path"
                )
                code_rows = cursor.fetchall()
                cursor.execute(
                    "SELECT scope_key, watermark_timestamp FROM agentic.kb_sync_state WHERE source_name = 'github' ORDER BY scope_key"
                )
                github_rows = cursor.fetchall()
                cursor.execute(
                    "SELECT cursor_text, watermark_timestamp FROM agentic.kb_sync_state WHERE source_name = 'project' AND scope_key = %s",
                    (project_scope_key(),),
                )
                project_row = cursor.fetchone()

        self.assertEqual(result["docs"]["changed_paths"], ("AGENTS.md",))
        self.assertEqual(result["docs"]["deleted_paths"], ("README.md",))
        self.assertEqual(result["code"]["changed_paths"], ("source/common/renamedAlpha.ts",))
        self.assertEqual(result["code"]["deleted_paths"], ("source/common/alpha.ts",))
        self.assertEqual([(row[0], row[1]) for row in doc_rows], [("AGENTS.md", 0)])
        self.assertIn("changed after import", doc_rows[0][2])
        self.assertEqual(doc_rows[0][3], head_commit)
        self.assertEqual([row[0] for row in code_rows], ["source/common/renamedAlpha.ts"])
        self.assertEqual(code_rows[0][1], head_commit)
        self.assertEqual(
            github_bounds["bounds"].updated_since,
            datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
        )
        self.assertEqual(project_bounds["bounds"].after_cursor, "cursor-seeded")
        self.assertEqual(project_row[0], "cursor-next")
        github_rows_by_scope = {scope_key: watermark_timestamp for scope_key, watermark_timestamp in github_rows}
        self.assertEqual(github_rows_by_scope[github_scope_key("issues")], datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc))
        self.assertEqual(github_rows_by_scope[github_scope_key("pulls")], datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc))
        self.assertEqual(github_rows_by_scope[github_scope_key("issue_comments")], datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc))
        self.assertEqual(github_rows_by_scope[github_scope_key("review_comments")], datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc))

    def test_sync_changed_fails_before_writing_when_github_baseline_is_incomplete(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nbaseline\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")
            self._seed_imported_baseline(workspace, baseline_commit, github_watermarks={"issues": None})

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
                with self.assertRaisesRegex(sync.SyncCommandError, "github baseline with watermark for issues"):
                    sync.sync_changed(
                        workspace,
                        config=AgenticConfig(
                            database_url=self.database_url,
                            ollama_base_url="http://ollama:11434",
                            ollama_embed_model="all-minilm",
                            github_token="token",
                        ),
                    )

    def test_sync_changed_docs_skips_unchanged_candidate_without_rewriting_row_metadata(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nbaseline\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")
            self._seed_imported_baseline(workspace, baseline_commit)

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT repo_commit_hash, source_updated_at, updated_at FROM agentic.kb_documents WHERE source_path = %s AND chunk_index = 0",
                    ("README.md",),
                )
                baseline_row = cursor.fetchone()

            subprocess.run(
                ["chmod", "+x", "README.md"],
                cwd=workspace,
                check=True,
                capture_output=True,
                text=True,
            )

            head_commit = self._git_commit_all(workspace, "mode-only docs delta")

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
                with patch.object(
                    docs.PostgresDocsStore,
                    "list_document_versions",
                    autospec=True,
                    wraps=docs.PostgresDocsStore.list_document_versions,
                ) as list_versions:
                    with patch(
                        "agentic_kb.commands.sync.ingest_github",
                        side_effect=lambda **kwargs: self._fake_github_result(kwargs["bounds"]),
                    ):
                        with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=self._noop_project_ingest):
                            result = sync.sync_changed(
                                workspace,
                                config=AgenticConfig(
                                    database_url=self.database_url,
                                    ollama_base_url="http://ollama:11434",
                                    ollama_embed_model="all-minilm",
                                    github_token="token",
                                ),
                            )

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT repo_commit_hash, source_updated_at, updated_at FROM agentic.kb_documents WHERE source_path = %s AND chunk_index = 0",
                    ("README.md",),
                )
                row = cursor.fetchone()
                cursor.execute(
                    "SELECT repo_commit_hash, metadata FROM agentic.kb_sync_state WHERE source_name = 'docs' AND scope_key = %s",
                    (repo_scope_key(),),
                )
                docs_state = cursor.fetchone()

        list_versions.assert_called_once()
        self.assertEqual(list_versions.call_args.args[1], ("README.md",))
        self.assertEqual(result["docs"]["changed_paths"], ("README.md",))
        self.assertEqual(result["docs"]["updated_paths"], ())
        self.assertEqual(result["docs"]["skipped_paths"], ("README.md",))
        self.assertEqual(result["docs"]["processed_count"], 0)
        self.assertEqual(row[0], baseline_commit)
        self.assertEqual(row[1], baseline_row[1])
        self.assertEqual(row[2], baseline_row[2])
        self.assertEqual(docs_state[0], head_commit)
        self.assertEqual(docs_state[1]["candidate_paths"], ["README.md"])
        self.assertEqual(docs_state[1]["updated_paths"], [])
        self.assertEqual(docs_state[1]["skipped_paths"], ["README.md"])

    def test_sync_changed_allows_compatible_imported_snapshot_manifest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nbaseline\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")
            self._seed_imported_baseline(workspace, baseline_commit)
            _replace_imported_snapshot_manifest(
                self.connection,
                _manifest_fixture(
                    snapshot_name="compatible-imported",
                    contract={
                        "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                        "embedding_model": "all-minilm",
                        "embedding_dimension": 384,
                    },
                ),
                embedding_model="all-minilm",
            )

            sync.ensure_sync_changed_snapshot_compatibility(
                self.database_url,
                AgenticConfig(
                    database_url=self.database_url,
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    def test_sync_changed_blocks_incompatible_imported_snapshot_manifest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nbaseline\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")
            self._seed_imported_baseline(workspace, baseline_commit)
            _replace_imported_snapshot_manifest(
                self.connection,
                _manifest_fixture(
                    snapshot_name="incompatible-imported",
                    contract={
                        "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                        "embedding_model": "different-model",
                        "embedding_dimension": 384,
                    },
                ),
                embedding_model="different-model",
            )

            with self.assertRaisesRegex(sync.SyncCommandError, "embedding model"):
                sync.ensure_sync_changed_snapshot_compatibility(
                    self.database_url,
                    AgenticConfig(
                        database_url=self.database_url,
                        ollama_base_url="http://ollama:11434",
                        ollama_embed_model="all-minilm",
                        github_token="token",
                    ),
                )

    def test_sync_changed_blocks_legacy_imported_snapshot_manifest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nbaseline\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            baseline_commit = self._git_commit_all(workspace, "baseline snapshot")
            self._seed_imported_baseline(workspace, baseline_commit)
            _replace_imported_snapshot_manifest(
                self.connection,
                _manifest_fixture(
                    snapshot_name="legacy-imported",
                    contract=None,
                    legacy=True,
                ),
                embedding_model="legacy-model",
            )

            with self.assertRaisesRegex(sync.SyncCommandError, "legacy embedding_model-only manifests are unsupported"):
                sync.ensure_sync_changed_snapshot_compatibility(
                    self.database_url,
                    AgenticConfig(
                        database_url=self.database_url,
                        ollama_base_url="http://ollama:11434",
                        ollama_embed_model="all-minilm",
                        github_token="token",
                    ),
                )

    def test_sync_all_stops_after_first_failure_and_persists_only_attempted_sources(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "README.md", "# Readme\n\nfirst\n")
            self._write_file(workspace / "source/common/alpha.ts", "export const Alpha = 1;\n")
            self._git_commit_all(workspace, "baseline")

            with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
                with patch("agentic_kb.commands.sync.ingest_code", side_effect=sync.SyncCommandError("code failed")):
                    with self.assertRaises(sync.SyncAllFailure):
                        sync.sync_all(
                            workspace,
                            config=AgenticConfig(
                                database_url=self.database_url,
                                ollama_base_url="http://ollama:11434",
                                ollama_embed_model="all-minilm",
                                github_token="token",
                            ),
                        )

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT source_name, scope_key, last_succeeded_at FROM agentic.kb_sync_state ORDER BY source_name, scope_key"
                )
                rows = cursor.fetchall()

        rows_by_source = {(source_name, scope_key): last_succeeded_at for source_name, scope_key, last_succeeded_at in rows}
        self.assertIn(("docs", repo_scope_key()), rows_by_source)
        self.assertIn(("code", repo_scope_key()), rows_by_source)
        self.assertNotIn(("github", github_scope_key("issues")), rows_by_source)
        self.assertNotIn(("project", project_scope_key()), rows_by_source)
        self.assertIsNotNone(rows_by_source[("docs", repo_scope_key())])
        self.assertIsNone(rows_by_source[("code", repo_scope_key())])

    def _seed_imported_baseline(
        self,
        workspace: Path,
        baseline_commit: str,
        *,
        github_watermarks=None,
        project_cursor_text="cursor-seeded",
    ) -> None:
        github_watermarks = github_watermarks or {}
        docs_store = docs.PostgresDocsStore.from_database_url(self.database_url)
        code_store = code.PostgresCodeChunksStore.from_database_url(self.database_url)
        sync_store = sync.PostgresSyncStateStore.from_database_url(self.database_url)
        self.addCleanup(docs_store.close)
        self.addCleanup(code_store.close)
        self.addCleanup(sync_store.close)

        prepared_docs = docs.prepare_documents(
            workspace,
            source_paths=["README.md"],
            embedding_client=FakeEmbeddingClient(),
            repo_commit_hash=baseline_commit,
        )
        docs_store.replace_documents_for_paths(["README.md"], prepared_docs)
        code.ingest_code(
            workspace,
            embedding_client=FakeEmbeddingClient(),
            code_store=code_store,
            source_paths=["source/common/alpha.ts"],
            repo_commit_hash=baseline_commit,
            run_mode="targeted",
            prune_missing=False,
        )
        sync_store.upsert_sync_states(
            [
                PreparedSyncState(
                    id=deterministic_sync_state_id("docs", repo_scope_key()),
                    source_name="docs",
                    scope_key=repo_scope_key(),
                    repo_commit_hash=baseline_commit,
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 10, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"repo": DEFAULT_SYNC_REPO},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("code", repo_scope_key()),
                    source_name="code",
                    scope_key=repo_scope_key(),
                    repo_commit_hash=baseline_commit,
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 10, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"repo": DEFAULT_SYNC_REPO},
                ),
                *self._build_github_states(github_watermarks),
                PreparedSyncState(
                    id=deterministic_sync_state_id("project", project_scope_key()),
                    source_name="project",
                    scope_key=project_scope_key(),
                    repo_commit_hash=None,
                    cursor_text=project_cursor_text,
                    watermark_text="2026-03-29T08:30:00Z",
                    watermark_timestamp=datetime(2026, 3, 29, 8, 30, tzinfo=timezone.utc),
                    schema_version=None,
                    last_attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 10, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"project_owner": DEFAULT_PROJECT_OWNER, "project_number": DEFAULT_PROJECT_NUMBER},
                ),
            ]
        )

    def _build_github_states(self, github_watermarks):
        states = []
        defaults = {
            "issues": datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
            "issue_comments": datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
            "pulls": datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            "review_comments": datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        }
        for stream_name in GITHUB_STREAM_NAMES:
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
                    last_attempted_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    last_succeeded_at=datetime(2026, 3, 29, 10, 1, tzinfo=timezone.utc),
                    last_error=None,
                    metadata={"repo": DEFAULT_SYNC_REPO, "stream_name": stream_name},
                )
            )
        return states

    def _fake_github_result(self, bounds):
        return github.GithubIngestResult(
            repo=DEFAULT_SYNC_REPO,
            bounds=bounds,
            stream_progress={
                "issues": github.GithubStreamProgress(
                    stream_name="issues",
                    pages_fetched=1,
                    hit_bound=False,
                    latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                    issues_written=1,
                    issue_comments_written=0,
                    prs_written=0,
                    pr_comments_written=0,
                ),
                "pulls": github.GithubStreamProgress(
                    stream_name="pulls",
                    pages_fetched=1,
                    hit_bound=False,
                    latest_source_updated_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                    issues_written=0,
                    issue_comments_written=0,
                    prs_written=1,
                    pr_comments_written=0,
                ),
                "issue_comments": github.GithubStreamProgress(
                    stream_name="issue_comments",
                    pages_fetched=1,
                    hit_bound=False,
                    latest_source_updated_at=datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc),
                    issues_written=0,
                    issue_comments_written=1,
                    prs_written=0,
                    pr_comments_written=0,
                ),
                "review_comments": github.GithubStreamProgress(
                    stream_name="review_comments",
                    pages_fetched=1,
                    hit_bound=False,
                    latest_source_updated_at=datetime(2026, 3, 29, 12, 30, tzinfo=timezone.utc),
                    issues_written=0,
                    issue_comments_written=0,
                    prs_written=0,
                    pr_comments_written=1,
                ),
            },
            issues_written=1,
            issue_comments_written=1,
            prs_written=1,
            pr_comments_written=1,
        )

    def _noop_project_ingest(self, **kwargs):
        return project.ProjectIngestResult(
            project_owner=DEFAULT_PROJECT_OWNER,
            project_number=DEFAULT_PROJECT_NUMBER,
            project_title="Daedalus Maintenance",
            project_url="https://github.com/orgs/DripDropz/projects/5",
            bounds=kwargs["bounds"],
            pages_fetched=0,
            hit_bound=False,
            final_cursor=kwargs["bounds"].after_cursor,
            latest_source_updated_at=None,
            rows_written=0,
        )

    def _git_init(self, workspace: Path) -> None:
        subprocess.run(["git", "init"], cwd=workspace, check=True, capture_output=True, text=True)
        subprocess.run(["git", "checkout", "-b", "main"], cwd=workspace, check=True, capture_output=True, text=True)

    def _git_commit_all(self, workspace: Path, message: str) -> str:
        subprocess.run(["git", "add", "."], cwd=workspace, check=True, capture_output=True, text=True)
        subprocess.run(
            ["git", "-c", "user.name=Agent", "-c", "user.email=agent@example.com", "commit", "-m", message],
            cwd=workspace,
            check=True,
            capture_output=True,
            text=True,
        )
        completed = subprocess.run(["git", "rev-parse", "HEAD"], cwd=workspace, check=True, capture_output=True, text=True)
        return completed.stdout.strip()

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


def _bootstrap_database(connection) -> None:
    init_sql = _sanitized_sql(SCHEMA_DIR / "init.sql")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def _sanitized_sql(path: Path) -> str:
    lines = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip().startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


def _manifest_fixture(snapshot_name: str, contract, *, legacy=False) -> dict:
    manifest = {
        "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
        "schema_version": 1,
        "snapshot_name": snapshot_name,
        "snapshot_created_at": "2026-03-29T10:00:00Z",
        "artifact": {
            "filename": f"{snapshot_name}.dump",
            "dump_format": "postgresql_custom",
            "compression": {"algorithm": "gzip", "level": 6},
            "size_bytes": 1,
            "content_hash": "sha256:" + "4" * 64,
        },
        "repo": {"name": DEFAULT_SYNC_REPO, "docs_commit_hash": None, "code_commit_hash": None},
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
            "project": {
                "owner": DEFAULT_PROJECT_OWNER,
                "number": DEFAULT_PROJECT_NUMBER,
                "cursor": None,
                "updated_at_watermark": None,
            },
        },
    }
    if legacy:
        manifest["embedding_model"] = "legacy-model"
    else:
        manifest["embedding_contract"] = contract
    return manifest


def _replace_imported_snapshot_manifest(connection, manifest: dict, *, embedding_model: str) -> None:
    snapshot_name = manifest["snapshot_name"]
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DELETE FROM agentic.kb_snapshot_manifest")
            cursor.execute(
                """
                INSERT INTO agentic.kb_snapshot_manifest (
                    id,
                    snapshot_name,
                    schema_version,
                    snapshot_created_at,
                    embedding_model,
                    entity_counts,
                    github_watermarks,
                    manifest,
                    source_path,
                    content_hash,
                    imported_at
                ) VALUES (
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    '{}'::jsonb,
                    '{}'::jsonb,
                    %s::jsonb,
                    %s,
                    %s,
                    %s
                )
                """,
                (
                    f"snapshot-manifest:{snapshot_name}",
                    snapshot_name,
                    1,
                    datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    embedding_model,
                    json.dumps(manifest),
                    f"/tmp/{snapshot_name}.manifest.json",
                    "sha256:" + "4" * 64,
                    datetime(2026, 3, 29, 10, 5, tzinfo=timezone.utc),
                ),
            )


if __name__ == "__main__":
    unittest.main()
