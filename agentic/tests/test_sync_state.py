from __future__ import annotations

import os
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec

from agentic_kb.ingest import github, project
import agentic_kb.ingest as ingest_exports
from agentic_kb.sync import state


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"


class SyncStateHelpersTests(unittest.TestCase):
    def test_deterministic_ids_scope_keys_and_lookup_helpers(self):
        record = state.SyncStateRecord(
            id="sync-state:github:repo:DripDropz/daedalus:issues",
            source_name="github",
            scope_key="repo:DripDropz/daedalus:issues",
            repo_commit_hash=None,
            cursor_text=None,
            watermark_text="2026-03-28T12:00:00Z",
            watermark_timestamp=datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc),
            schema_version=None,
            last_attempted_at=datetime(2026, 3, 28, 12, 1, tzinfo=timezone.utc),
            last_succeeded_at=datetime(2026, 3, 28, 12, 2, tzinfo=timezone.utc),
            last_error=None,
            metadata={"repo": "DripDropz/daedalus"},
            created_at=datetime(2026, 3, 28, 12, 1, tzinfo=timezone.utc),
            updated_at=datetime(2026, 3, 28, 12, 2, tzinfo=timezone.utc),
        )

        self.assertEqual(
            state.deterministic_sync_state_id("docs", "repo:DripDropz/daedalus"),
            "sync-state:docs:repo:DripDropz/daedalus",
        )
        self.assertEqual(state.repo_scope_key(), "repo:DripDropz/daedalus")
        self.assertEqual(
            state.github_scope_key("issues"),
            "repo:DripDropz/daedalus:issues",
        )
        self.assertEqual(state.project_scope_key(), "project:DripDropz/5")
        self.assertEqual(
            state.github_stream_updated_since(record),
            datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc),
        )
        self.assertEqual(state.project_cursor(record), None)

    def test_ingest_package_reexports_sync_lookup_helpers(self):
        self.assertIs(ingest_exports.get_sync_state, state.get_sync_state)
        self.assertIs(ingest_exports.list_sync_states, state.list_sync_states)

    def test_build_docs_and_code_sync_state_keep_watermarks_unset(self):
        attempted_at = datetime(2026, 3, 28, 14, 0, tzinfo=timezone.utc)
        succeeded_at = datetime(2026, 3, 28, 14, 1, tzinfo=timezone.utc)
        docs_result = type(
            "DocsResult",
            (),
            {
                "repo_commit_hash": "docs-commit",
                "processed_count": 2,
                "source_paths": ("AGENTS.md", "README.md"),
            },
        )()
        code_result = type(
            "CodeResult",
            (),
            {
                "repo_commit_hash": "code-commit",
                "processed_file_count": 3,
                "chunk_count": 5,
                "source_paths": ("source/main/index.ts",),
            },
        )()

        docs_state = state.build_docs_sync_state(
            docs_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )
        code_state = state.build_code_sync_state(
            code_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )

        self.assertEqual(docs_state.repo_commit_hash, "docs-commit")
        self.assertIsNone(docs_state.cursor_text)
        self.assertIsNone(docs_state.watermark_text)
        self.assertIsNone(docs_state.watermark_timestamp)
        self.assertEqual(docs_state.metadata["processed_count"], 2)
        self.assertEqual(code_state.repo_commit_hash, "code-commit")
        self.assertIsNone(code_state.watermark_timestamp)
        self.assertEqual(code_state.metadata["chunk_count"], 5)

    def test_build_github_and_project_sync_states_capture_watermarks_and_cursor(self):
        attempted_at = datetime(2026, 3, 28, 15, 0, tzinfo=timezone.utc)
        succeeded_at = datetime(2026, 3, 28, 15, 1, tzinfo=timezone.utc)
        github_result = github.GithubIngestResult(
            repo="DripDropz/daedalus",
            bounds=github.GithubFetchBounds(
                repo="DripDropz/daedalus",
                updated_since=datetime(2026, 3, 27, 0, 0, tzinfo=timezone.utc),
                page_size=25,
                max_pages=2,
            ),
            stream_progress={
                name: github.GithubStreamProgress(
                    stream_name=name,
                    pages_fetched=1,
                    hit_bound=name == "issues",
                    latest_source_updated_at=datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc)
                    if name in {"issues", "pulls"}
                    else None,
                    issues_written=1 if name == "issues" else 0,
                    issue_comments_written=1 if name == "issue_comments" else 0,
                    prs_written=1 if name == "pulls" else 0,
                    pr_comments_written=1 if name == "review_comments" else 0,
                )
                for name in github.STREAM_ORDER
            },
            issues_written=1,
            issue_comments_written=1,
            prs_written=1,
            pr_comments_written=1,
        )
        project_result = project.ProjectIngestResult(
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url="https://github.com/orgs/DripDropz/projects/5",
            bounds=project.ProjectFetchBounds(
                project_owner="DripDropz",
                project_number=5,
                page_size=10,
                max_pages=1,
                after_cursor="cursor-before",
            ),
            pages_fetched=1,
            hit_bound=True,
            final_cursor="cursor-after",
            latest_source_updated_at=datetime(2026, 3, 28, 16, 0, tzinfo=timezone.utc),
            rows_written=3,
        )

        github_states = state.build_github_sync_state_updates(
            github_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )
        project_state = state.build_project_sync_state(
            project_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )

        self.assertEqual(len(github_states), 4)
        issues_state = next(item for item in github_states if item.metadata["stream_name"] == "issues")
        self.assertEqual(issues_state.scope_key, "repo:DripDropz/daedalus:issues")
        self.assertEqual(issues_state.watermark_text, "2026-03-28T12:00:00Z")
        self.assertEqual(issues_state.metadata["updated_since"], "2026-03-27T00:00:00Z")
        self.assertEqual(project_state.cursor_text, "cursor-after")
        self.assertEqual(project_state.watermark_text, "2026-03-28T16:00:00Z")
        self.assertEqual(project_state.metadata["after_cursor"], "cursor-before")

    def test_in_memory_store_records_attempts_failures_and_upserts(self):
        store = state.InMemorySyncStateStore()
        attempted_at = datetime(2026, 3, 28, 17, 0, tzinfo=timezone.utc)
        succeeded_at = datetime(2026, 3, 28, 17, 1, tzinfo=timezone.utc)

        store.record_attempts(
            [
                state.build_sync_attempt(
                    "docs",
                    state.repo_scope_key(),
                    attempted_at=attempted_at,
                    metadata={"repo": "DripDropz/daedalus"},
                )
            ]
        )
        record_after_attempt = store.get_sync_state("docs", state.repo_scope_key())
        self.assertEqual(record_after_attempt.last_attempted_at, attempted_at)
        self.assertIsNone(record_after_attempt.last_succeeded_at)

        store.record_failures(
            [
                state.build_sync_failure(
                    "docs",
                    state.repo_scope_key(),
                    attempted_at=attempted_at,
                    error=RuntimeError("first failure"),
                    metadata={"repo": "DripDropz/daedalus", "phase": "attempt"},
                )
            ]
        )
        failed_record = store.get_sync_state("docs", state.repo_scope_key())
        self.assertEqual(failed_record.last_error, "first failure")
        self.assertEqual(failed_record.metadata["phase"], "attempt")

        docs_state = state.build_docs_sync_state(
            type(
                "DocsResult",
                (),
                {
                    "repo_commit_hash": "commit-c",
                    "processed_count": 1,
                    "source_paths": ("AGENTS.md",),
                },
            )(),
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )
        state.persist_sync_state_updates(store, [docs_state])
        success_record = store.get_sync_state("docs", state.repo_scope_key())
        self.assertEqual(success_record.repo_commit_hash, "commit-c")
        self.assertEqual(success_record.last_succeeded_at, succeeded_at)
        self.assertIsNone(success_record.last_error)

        listed = state.list_sync_states(store, source_name="docs")
        self.assertEqual(len(listed), 1)


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class PostgresSyncStateStoreTests(unittest.TestCase):
    def setUp(self):
        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.store = state.PostgresSyncStateStore.from_database_url(self.database_url)
        self.addCleanup(self.store.close)
        self._truncate_table()

    def _truncate_table(self):
        with self.store._connection.transaction():
            with self.store._connection.cursor() as cursor:
                cursor.execute("TRUNCATE TABLE agentic.kb_sync_state")

    def test_postgres_store_round_trip_attempt_failure_and_upsert(self):
        attempted_at = datetime(2026, 3, 28, 18, 0, tzinfo=timezone.utc)
        succeeded_at = datetime(2026, 3, 28, 18, 2, tzinfo=timezone.utc)
        scope_key = state.github_scope_key("issues")

        self.store.record_attempts(
            [
                state.build_sync_attempt(
                    "github",
                    scope_key,
                    attempted_at=attempted_at,
                    metadata={"repo": "DripDropz/daedalus", "stream_name": "issues"},
                )
            ]
        )
        attempt_record = self.store.get_sync_state("github", scope_key)
        self.assertEqual(attempt_record.last_attempted_at, attempted_at)
        self.assertEqual(attempt_record.metadata["stream_name"], "issues")

        self.store.record_failures(
            [
                state.build_sync_failure(
                    "github",
                    scope_key,
                    attempted_at=attempted_at,
                    error="project scope missing",
                    metadata={"repo": "DripDropz/daedalus", "stream_name": "issues"},
                )
            ]
        )
        failure_record = self.store.get_sync_state("github", scope_key)
        self.assertEqual(failure_record.last_error, "project scope missing")

        github_result = github.GithubIngestResult(
            repo="DripDropz/daedalus",
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", page_size=50, max_pages=1),
            stream_progress={
                "issues": github.GithubStreamProgress(
                    stream_name="issues",
                    pages_fetched=1,
                    hit_bound=True,
                    latest_source_updated_at=datetime(2026, 3, 28, 18, 1, tzinfo=timezone.utc),
                    issues_written=2,
                    issue_comments_written=0,
                    prs_written=0,
                    pr_comments_written=0,
                )
            },
            issues_written=2,
            issue_comments_written=0,
            prs_written=0,
            pr_comments_written=0,
        )
        updates = state.build_github_sync_state_updates(
            github_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        )
        self.store.upsert_sync_states(updates)

        persisted = self.store.get_sync_state("github", scope_key)
        self.assertEqual(persisted.id, "sync-state:github:repo:DripDropz/daedalus:issues")
        self.assertEqual(persisted.watermark_text, "2026-03-28T18:01:00Z")
        self.assertEqual(
            persisted.watermark_timestamp,
            datetime(2026, 3, 28, 18, 1, tzinfo=timezone.utc),
        )
        self.assertEqual(persisted.last_attempted_at, attempted_at)
        self.assertEqual(persisted.last_succeeded_at, succeeded_at)
        self.assertIsNone(persisted.last_error)
        self.assertEqual(persisted.metadata["pages_fetched"], 1)
        self.assertTrue(persisted.metadata["hit_bound"])

        listed = self.store.list_sync_states(source_name="github")
        self.assertEqual(len(listed), 1)


if __name__ == "__main__":
    unittest.main()
