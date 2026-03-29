from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import status


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV)
    and find_spec("psycopg") is not None
    and shutil.which("psql") is not None,
    "requires AGENTIC_TEST_DATABASE_URL, psycopg, and psql",
)
class SchemaBootstrapTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)

    def test_init_sql_bootstraps_schema_and_indexes_via_shipped_include_path(self):
        _reset_agentic_schema(self.connection)
        _bootstrap_database_via_psql(self.database_url)

        inspection = status.inspect_database(self.database_url)

        self.assertEqual(inspection.applied_versions, status.EXPECTED_MIGRATION_VERSIONS)
        self.assertEqual(set(inspection.tables), set(status.EXPECTED_AGENTIC_TABLES))
        self.assertTrue(
            set(status.expected_searchable_tables()).issubset(set(inspection.tables))
        )
        self.assertTrue(
            set(status.expected_searchable_indexes()).issubset(set(inspection.indexes))
        )
        self.assertEqual(set(inspection.row_counts), set(status.expected_searchable_tables()))
        self.assertTrue(all(count == 0 for count in inspection.row_counts.values()))
        self.assertEqual(inspection.sync_summaries, ())


def _reset_agentic_schema(connection) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")


def _bootstrap_database_via_psql(database_url: str) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
        bootstrap_dir = Path(temp_dir)
        includes_dir = bootstrap_dir / "includes"
        includes_dir.mkdir()
        shutil.copyfile(SCHEMA_DIR / "init.sql", bootstrap_dir / "init.sql")
        shutil.copyfile(
            SCHEMA_DIR / "create_indexes.sql",
            includes_dir / "create_indexes.task-203.sql",
        )

        subprocess.run(
            [
                "psql",
                database_url,
                "-v",
                "ON_ERROR_STOP=1",
                "-f",
                str(bootstrap_dir / "init.sql"),
            ],
            check=True,
            capture_output=True,
            text=True,
        )


if __name__ == "__main__":
    unittest.main()
