from __future__ import annotations

import os
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import status


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class StatusCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        _seed_status_fixture(self.connection)
        self.connection.commit()

    def test_inspect_database_reports_current_schema_indexes_counts_and_sync_state(self):
        inspection = status.inspect_database(self.database_url)

        self.assertEqual(inspection.applied_versions, (1, 2, 3))
        self.assertTrue(set(status.EXPECTED_AGENTIC_TABLES).issubset(set(inspection.tables)))
        self.assertTrue(set(status.expected_searchable_indexes()).issubset(set(inspection.indexes)))
        self.assertEqual(inspection.row_counts["agentic.kb_documents"], 1)
        docs_summary = next(summary for summary in inspection.sync_summaries if summary.source_name == "docs")
        self.assertEqual(docs_summary.row_count, 1)
        self.assertEqual(docs_summary.error_count, 0)


def _bootstrap_database(connection) -> None:
    init_sql = _sanitized_sql(SCHEMA_DIR / "init.sql")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def _seed_status_fixture(connection) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_documents CASCADE")
            cursor.execute(
                """
                INSERT INTO agentic.kb_documents (
                    id,
                    source_domain,
                    doc_kind,
                    source_path,
                    title,
                    heading_path,
                    chunk_index,
                    content,
                    preview_text,
                    content_hash
                ) VALUES (
                    'docs:status-db#0',
                    'docs',
                    'workflow',
                    '.agent/workflows/agentic-kb.md',
                    'status workflow',
                    '[]'::jsonb,
                    0,
                    'workflow content',
                    'workflow content',
                    'status-doc-hash'
                )
                """
            )
            cursor.execute(
                """
                INSERT INTO agentic.kb_sync_state (
                    id,
                    source_name,
                    scope_key,
                    repo_commit_hash,
                    last_attempted_at,
                    last_succeeded_at,
                    metadata
                ) VALUES (
                    'sync-state:docs:repo:DripDropz/daedalus:status-db',
                    'docs',
                    'repo:DripDropz/daedalus:status-db',
                    'deadbeef',
                    %s,
                    %s,
                    '{"repo": "DripDropz/daedalus"}'::jsonb
                )
                """,
                (
                    datetime(2026, 3, 28, 23, 10, tzinfo=timezone.utc),
                    datetime(2026, 3, 28, 23, 11, tzinfo=timezone.utc),
                ),
            )


def _sanitized_sql(path: Path) -> str:
    lines = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


if __name__ == "__main__":
    unittest.main()
