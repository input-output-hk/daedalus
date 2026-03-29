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
from agentic_kb.sync.state import (
    InMemorySyncStateStore,
    PreparedSyncState,
    build_docs_sync_state,
    deterministic_sync_state_id,
    github_scope_key,
    project_scope_key,
    repo_scope_key,
)

try:
    from agentic_kb.ingest import code
except ImportError:  # pragma: no cover - local env may omit tree-sitter extras
    code = None


class FakeEmbeddingClient:
    def embed_texts(self, texts):
        return [[1.0] * 384 for _ in texts]


class SyncCommandTests(unittest.TestCase):
    def test_cli_routes_sync_changed_to_real_handler_and_keeps_other_verbs_placeholder(self):
        parser = cli.build_parser()

        changed_args = parser.parse_args(["sync", "changed"])
        docs_args = parser.parse_args(["sync", "docs"])

        self.assertIs(changed_args.handler, sync.run_sync_changed)
        self.assertIs(docs_args.handler, sync._run_sync_placeholder)

    def test_placeholder_sync_verbs_remain_deferred(self):
        stdout = io.StringIO()
        with redirect_stdout(stdout):
            exit_code = sync._run_sync_placeholder(Namespace(placeholder_name="docs"))

        self.assertEqual(exit_code, 2)
        self.assertIn("task-701", stdout.getvalue())

    def test_load_required_baselines_rejects_missing_rows(self):
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

        with self.assertRaisesRegex(sync.SyncCommandError, "missing code:repo:DripDropz/daedalus"):
            sync.load_required_baselines(store)

    def test_compute_docs_delta_uses_baseline_commit_and_deletion_filtering(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._git_init(workspace)
            self._write_file(workspace / "AGENTS.md", "# Agents\n\nfirst\n")
            self._write_file(workspace / "README.md", "# Readme\n\nfirst\n")
            self._write_file(workspace / "docs/ignored.md", "# Ignored\n")
            baseline = self._git_commit_all(workspace, "baseline")

            self._write_file(workspace / "AGENTS.md", "# Agents\n\nsecond\n")
            (workspace / "README.md").unlink()
            self._write_file(workspace / "docs/ignored.md", "# Ignored\n\nchanged\n")
            self._git_commit_all(workspace, "delta")

            delta = sync.compute_docs_delta(workspace, baseline)

        self.assertEqual(delta.baseline_commit, baseline)
        self.assertEqual(delta.changed_paths, ("AGENTS.md",))
        self.assertEqual(delta.deleted_paths, ("README.md",))

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
                docs.prepare_documents(
                    workspace,
                    source_paths=["AGENTS.md"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash="baseline-commit",
                )
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
        self.assertEqual(result["deleted_paths"], ("README.md",))
        self.assertIn(("AGENTS.md", 0), docs_store.rows_by_key)
        self.assertNotIn(("README.md", 0), docs_store.rows_by_key)
        self.assertEqual(docs_store.rows_by_key[("AGENTS.md", 0)]["repo_commit_hash"], "head-commit")

    def test_sync_github_changed_uses_supported_streams_and_marks_deferred_streams(self):
        sync_store = InMemorySyncStateStore()
        baselines = self._seed_all_required_baselines(sync_store)
        captured = {}

        def fake_ingest_supported_github_streams(**kwargs):
            captured.update(kwargs)
            return github.GithubIngestResult(
                repo="DripDropz/daedalus",
                bounds=kwargs["bounds"],
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
                },
                issues_written=1,
                issue_comments_written=1,
                prs_written=0,
                pr_comments_written=0,
            )

        with patch("agentic_kb.commands.sync._ingest_supported_github_streams", side_effect=fake_ingest_supported_github_streams):
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

        self.assertEqual(result["synced_streams"], sync.SUPPORTED_GITHUB_BOOTSTRAP_STREAMS)
        self.assertEqual(result["deferred_streams"], sync.DEFERRED_GITHUB_BOOTSTRAP_STREAMS)
        self.assertEqual(
            captured["bounds"].updated_since,
            datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc),
        )
        deferred_pulls = sync_store.get_sync_state("github", github_scope_key("pulls"))
        self.assertEqual(deferred_pulls.metadata["task_604_status"], "deferred")
        self.assertIn("follow-up work", deferred_pulls.metadata["task_604_note"])

    def test_load_required_baselines_rejects_missing_supported_github_watermark(self):
        sync_store = InMemorySyncStateStore()
        self._seed_all_required_baselines(sync_store, github_watermarks={"issues": None}, validate=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "missing github:repo:DripDropz/daedalus:issues watermark"):
            sync.load_required_baselines(sync_store)

    def test_load_required_baselines_rejects_missing_project_cursor(self):
        sync_store = InMemorySyncStateStore()
        self._seed_all_required_baselines(sync_store, project_cursor_text=None, validate=False)

        with self.assertRaisesRegex(sync.SyncCommandError, "missing project:project:DripDropz/5 cursor_text"):
            sync.load_required_baselines(sync_store)

    def test_sync_project_changed_uses_cursor_continuation(self):
        sync_store = InMemorySyncStateStore()
        baselines = self._seed_all_required_baselines(sync_store)
        captured = {}

        def fake_ingest_project_items(**kwargs):
            captured.update(kwargs)
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="cursor-next",
                latest_source_updated_at=datetime(2026, 3, 29, 13, 0, tzinfo=timezone.utc),
                rows_written=2,
            )

        with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
            result = sync._sync_project_changed(
                config=AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
                embedding_client=FakeEmbeddingClient(),
                project_store=project.InMemoryProjectItemsStore(),
                sync_store=sync_store,
                baseline=baselines["project"],
                attempted_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
            )

        self.assertEqual(captured["bounds"].after_cursor, "cursor-seeded")
        self.assertEqual(result["starting_cursor"], "cursor-seeded")
        self.assertEqual(result["final_cursor"], "cursor-next")

    def test_run_sync_changed_writes_errors_to_stderr_only(self):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch("agentic_kb.commands.sync.sync_changed", side_effect=sync.SyncCommandError("missing baseline")):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = sync.run_sync_changed(Namespace())

        self.assertEqual(exit_code, 1)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("sync changed failed: missing baseline", stderr.getvalue())

    def _seed_docs_baseline(self, store: InMemorySyncStateStore, *, repo_commit_hash: str):
        state = build_docs_sync_state(
            docs.DocsIngestResult(source_paths=("AGENTS.md",), processed_count=1, repo_commit_hash=repo_commit_hash),
            attempted_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            succeeded_at=datetime(2026, 3, 29, 9, 1, tzinfo=timezone.utc),
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
        github_states = [
            PreparedSyncState(
                id=deterministic_sync_state_id("github", github_scope_key(stream_name)),
                source_name="github",
                scope_key=github_scope_key(stream_name),
                repo_commit_hash=None,
                cursor_text=None,
                watermark_text=(
                    github_watermarks.get(stream_name).isoformat().replace("+00:00", "Z")
                    if stream_name in github_watermarks and github_watermarks[stream_name] is not None
                    else None
                    if stream_name in github_watermarks
                    else "2026-03-29T07:00:00Z"
                    if stream_name == "issues"
                    else "2026-03-29T08:00:00Z"
                    if stream_name == "issue_comments"
                    else None
                ),
                watermark_timestamp=(
                    github_watermarks.get(stream_name)
                    if stream_name in github_watermarks
                    else datetime(2026, 3, 29, 7, 0, tzinfo=timezone.utc)
                    if stream_name == "issues"
                    else datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc)
                    if stream_name == "issue_comments"
                    else None
                ),
                schema_version=None,
                last_attempted_at=datetime(2026, 3, 29, 8, 0, tzinfo=timezone.utc),
                last_succeeded_at=datetime(2026, 3, 29, 8, 1, tzinfo=timezone.utc),
                last_error=None,
                metadata={"repo": "DripDropz/daedalus", "stream_name": stream_name},
            )
            for stream_name in sync.GITHUB_STREAM_NAMES
        ]
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
        return sync.load_required_baselines(store)

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
