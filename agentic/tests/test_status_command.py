from __future__ import annotations

import unittest
from datetime import datetime, timezone
from unittest.mock import patch

from agentic_kb.commands import status
from agentic_kb.config import AgenticConfig


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
        inspect_database.assert_not_called()

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


if __name__ == "__main__":
    unittest.main()
