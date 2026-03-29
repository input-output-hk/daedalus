from __future__ import annotations

import unittest
import json
from datetime import datetime, timezone
from argparse import Namespace
from contextlib import redirect_stdout
import io
from unittest.mock import patch

from agentic_kb.commands import status
from agentic_kb.config import AgenticConfig
from agentic_kb.sync.state import InMemorySyncStateStore, PreparedSyncState, deterministic_sync_state_id, github_scope_key, project_scope_key, repo_scope_key
from agentic_kb.sync.staleness import FreshnessItem, FreshnessReport


class StatusCommandTests(unittest.TestCase):
    def setUp(self):
        self.config = AgenticConfig(
            database_url="postgresql://agentic:agentic@db:5432/agentic_kb",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

    @patch("agentic_kb.commands.status._path_exists", return_value=True)
    @patch(
        "agentic_kb.commands.status._check_ollama",
        return_value=(True, "reachable via http://ollama:11434/api/tags", True, "configured model available"),
    )
    @patch("agentic_kb.commands.status._check_tcp")
    @patch("agentic_kb.commands.status.inspect_database")
    def test_collect_status_report_includes_database_readiness(
        self,
        inspect_database,
        _check_tcp,
        _check_ollama,
        _path_exists,
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
        )

        report = status.collect_status_report(self.config, healthcheck=False)

        self.assertTrue(report.ok)
        self.assertIsNotNone(report.freshness)
        self.assertTrue(any(item.name == "schema migrations" and item.ok for item in report.database_items))
        self.assertTrue(any(item.name == "search indexes" and item.ok for item in report.database_items))
        self.assertTrue(any(item.name == "sync state summary" and item.detail == "0 rows recorded" for item in report.database_items))

    @patch("agentic_kb.commands.status._path_exists", return_value=True)
    @patch(
        "agentic_kb.commands.status._check_ollama",
        return_value=(True, "reachable via http://ollama:11434/api/tags", True, "configured model available"),
    )
    @patch("agentic_kb.commands.status._check_tcp")
    @patch("agentic_kb.commands.status.inspect_database")
    def test_collect_status_report_surfaces_database_inspection_failures(
        self,
        inspect_database,
        _check_tcp,
        _check_ollama,
        _path_exists,
    ):
        inspect_database.side_effect = status.StatusCommandError("connection refused")

        report = status.collect_status_report(self.config, healthcheck=False)

        self.assertFalse(report.ok)
        self.assertEqual(report.database_items[0].name, "database inspection")
        self.assertEqual(report.database_items[0].detail, "connection refused")

    @patch("agentic_kb.commands.status._path_exists", return_value=True)
    @patch(
        "agentic_kb.commands.status._check_ollama",
        return_value=(True, "reachable via http://ollama:11434/api/tags", True, "configured model available"),
    )
    @patch("agentic_kb.commands.status._check_tcp")
    @patch("agentic_kb.commands.status.inspect_database")
    def test_healthcheck_skips_database_inspection(
        self,
        inspect_database,
        _check_tcp,
        _check_ollama,
        _path_exists,
    ):
        report = status.collect_status_report(self.config, healthcheck=True)

        self.assertTrue(report.ok)
        self.assertEqual(report.database_items, ())
        self.assertIsNone(report.freshness)
        inspect_database.assert_not_called()

    @patch("agentic_kb.commands.status._collect_freshness")
    @patch("agentic_kb.commands.status._path_exists", return_value=True)
    @patch(
        "agentic_kb.commands.status._check_ollama",
        return_value=(True, "reachable via http://ollama:11434/api/tags", True, "configured model available"),
    )
    @patch("agentic_kb.commands.status._check_tcp")
    @patch("agentic_kb.commands.status.inspect_database")
    def test_stale_freshness_does_not_change_top_level_readiness(
        self,
        inspect_database,
        _check_tcp,
        _check_ollama,
        _path_exists,
        collect_freshness,
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
        )
        collect_freshness.return_value = FreshnessReport(
            up_to_date=False,
            items=(
                FreshnessItem(
                    name="docs",
                    status="stale",
                    detail="baseline commit differs from local HEAD",
                    baseline="abc",
                    observed="def",
                ),
            ),
        )

        report = status.collect_status_report(self.config, healthcheck=False)

        self.assertTrue(report.ok)
        self.assertFalse(report.freshness.up_to_date)
        self.assertEqual(report.freshness.items[0].status, "stale")

    def test_build_database_items_marks_missing_schema_objects(self):
        inspection = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2),
            tables=("agentic.kb_schema_migrations", "agentic.kb_documents", "agentic.kb_sync_state"),
            indexes=("kb_documents_bm25_idx",),
            row_counts={"agentic.kb_documents": 2},
            sync_summaries=(
                status.SyncSourceSummary(
                    source_name="docs",
                    row_count=1,
                    error_count=1,
                    last_attempted_at=datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc),
                    last_succeeded_at=None,
                ),
            ),
        )

        items = status.build_database_items(inspection, endpoint_detail="db:5432/agentic_kb")
        item_by_name = {item.name: item for item in items}

        self.assertFalse(item_by_name["schema migrations"].ok)
        self.assertIn("missing 3", item_by_name["schema migrations"].detail)
        self.assertFalse(item_by_name["agentic tables"].ok)
        self.assertFalse(item_by_name["search indexes"].ok)
        self.assertEqual(item_by_name["rows agentic.kb_documents"].detail, "2")
        self.assertEqual(
            item_by_name["sync docs"].detail,
            "rows=1, errors=1, last_attempted=2026-03-28T12:00:00Z, last_succeeded=never",
        )

    def test_serialize_status_report_returns_stable_json_shape(self):
        report = status.StatusReport(
            healthcheck=False,
            ok=True,
            config_items=(status.CheckResult(name="DATABASE_URL", ok=True, detail="db:5432/agentic_kb"),),
            environment_items=(),
            dependency_items=(),
            database_items=(),
            freshness=FreshnessReport(
                up_to_date=False,
                items=(
                    FreshnessItem(
                        name="project",
                        status="skipped",
                        detail="skipped because GITHUB_TOKEN is not configured",
                    ),
                ),
            ),
            notes=("note one",),
        )

        payload = status.serialize_status_report(report)

        self.assertEqual(list(payload.keys()), [
            "healthcheck",
            "ok",
            "config_items",
            "environment_items",
            "dependency_items",
            "database_items",
            "freshness",
            "notes",
        ])
        self.assertEqual(payload["config_items"][0], {
            "name": "DATABASE_URL",
            "ok": True,
            "detail": "db:5432/agentic_kb",
        })
        self.assertEqual(payload["freshness"], {
            "up_to_date": False,
            "items": [
                {
                    "name": "project",
                    "status": "skipped",
                    "detail": "skipped because GITHUB_TOKEN is not configured",
                    "baseline": None,
                    "observed": None,
                }
            ],
        })

    def test_collect_freshness_report_covers_fresh_stale_missing_and_skipped_sources(self):
        sync_store = InMemorySyncStateStore()
        timestamp = datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc)
        sync_store.upsert_sync_states(
            [
                PreparedSyncState(
                    id=deterministic_sync_state_id("docs", repo_scope_key()),
                    source_name="docs",
                    scope_key=repo_scope_key(),
                    repo_commit_hash="head123",
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("code", repo_scope_key()),
                    source_name="code",
                    scope_key=repo_scope_key(),
                    repo_commit_hash="old456",
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
            ]
        )

        report = status.collect_freshness_report(
            workspace_root="/tmp",
            sync_store=sync_store,
            github_token=None,
            head_commit_getter=lambda _root: "head123",
        )

        item_by_name = {item.name: item for item in report.items}
        self.assertEqual(item_by_name["docs"].status, "fresh")
        self.assertEqual(item_by_name["code"].status, "stale")
        self.assertEqual(item_by_name["github issues"].status, "skipped")
        self.assertEqual(item_by_name["project"].status, "skipped")

    def test_collect_freshness_report_covers_remote_and_imported_baseline_boundaries(self):
        sync_store = InMemorySyncStateStore()
        timestamp = datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc)
        sync_store.upsert_sync_states(
            [
                PreparedSyncState(
                    id=deterministic_sync_state_id("docs", repo_scope_key()),
                    source_name="docs",
                    scope_key=repo_scope_key(),
                    repo_commit_hash="old-head",
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("code", repo_scope_key()),
                    source_name="code",
                    scope_key=repo_scope_key(),
                    repo_commit_hash="old-head",
                    cursor_text=None,
                    watermark_text=None,
                    watermark_timestamp=None,
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("github", github_scope_key("issues")),
                    source_name="github",
                    scope_key=github_scope_key("issues"),
                    repo_commit_hash=None,
                    cursor_text=None,
                    watermark_text="2026-03-29T10:00:00Z",
                    watermark_timestamp=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("github", github_scope_key("pulls")),
                    source_name="github",
                    scope_key=github_scope_key("pulls"),
                    repo_commit_hash=None,
                    cursor_text=None,
                    watermark_text="2026-03-29T12:00:00Z",
                    watermark_timestamp=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
                PreparedSyncState(
                    id=deterministic_sync_state_id("project", project_scope_key()),
                    source_name="project",
                    scope_key=project_scope_key(),
                    repo_commit_hash=None,
                    cursor_text="cursor-123",
                    watermark_text="2026-03-29T09:00:00Z",
                    watermark_timestamp=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                    schema_version=None,
                    last_attempted_at=timestamp,
                    last_succeeded_at=timestamp,
                    last_error=None,
                    metadata={},
                ),
            ]
        )

        report = status.collect_freshness_report(
            workspace_root="/tmp",
            sync_store=sync_store,
            github_token="secret-token",
            head_commit_getter=lambda _root: "new-head",
            github_watermarks_fetcher=lambda **_kwargs: {
                "issues": datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
                "pulls": datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
                "issue_comments": None,
                "review_comments": None,
            },
            project_watermark_fetcher=lambda **_kwargs: datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
        )

        item_by_name = {item.name: item for item in report.items}
        self.assertFalse(report.up_to_date)
        self.assertEqual(item_by_name["docs"].status, "stale")
        self.assertEqual(item_by_name["code"].status, "stale")
        self.assertEqual(item_by_name["github issues"].status, "stale")
        self.assertEqual(item_by_name["github pulls"].status, "fresh")
        self.assertEqual(item_by_name["github issue_comments"].status, "missing_baseline")
        self.assertEqual(item_by_name["project"].status, "stale")

    @patch("agentic_kb.commands.status.collect_status_report")
    @patch("agentic_kb.commands.status.AgenticConfig.from_env")
    def test_run_status_json_emits_only_json(self, from_env, collect_status_report):
        from_env.return_value = object()
        collect_status_report.return_value = status.StatusReport(
            healthcheck=True,
            ok=True,
            config_items=(),
            environment_items=(),
            dependency_items=(),
            database_items=(),
            freshness=None,
            notes=("healthcheck mode",),
        )

        stdout = io.StringIO()
        with redirect_stdout(stdout):
            exit_code = status.run_status(Namespace(healthcheck=True, json=True))

        self.assertEqual(exit_code, 0)
        self.assertEqual(json.loads(stdout.getvalue()), {
            "healthcheck": True,
            "ok": True,
            "config_items": [],
            "environment_items": [],
            "dependency_items": [],
            "database_items": [],
            "freshness": None,
            "notes": ["healthcheck mode"],
        })


if __name__ == "__main__":
    unittest.main()
