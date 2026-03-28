from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import snapshot, status


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV)
    and find_spec("psycopg") is not None
    and shutil.which("pg_dump") is not None
    and shutil.which("pg_restore") is not None
    and shutil.which("psql") is not None,
    "requires AGENTIC_TEST_DATABASE_URL, psycopg, and PostgreSQL client tools",
)
class SnapshotCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        _seed_snapshot_fixture(self.connection)
        self.connection.commit()

    def test_export_and_destructive_import_round_trip_agentic_schema(self):
        before = status.inspect_database(self.database_url)
        self.assertEqual(before.row_counts["agentic.kb_documents"], 1)

        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-205-roundtrip.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            self.assertEqual(export_result.path, dump_path)
            self.assertTrue(dump_path.exists())

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_documents")

            mutated = status.inspect_database(self.database_url)
            self.assertEqual(mutated.row_counts["agentic.kb_documents"], 0)

            import_result = snapshot.import_snapshot(self.database_url, dump_path, confirmed=True)
            self.assertEqual(import_result.path, dump_path)

        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.applied_versions, (1, 2, 3))
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)
        docs_summary = next(summary for summary in restored.sync_summaries if summary.source_name == "docs")
        self.assertEqual(docs_summary.row_count, 1)

    def test_import_restricts_restore_to_agentic_schema_for_nonconforming_dump(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-205-full-db.dump"

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("DROP TABLE IF EXISTS public.task205_external_restore")
                    cursor.execute("CREATE TABLE public.task205_external_restore (id integer PRIMARY KEY)")
                    cursor.execute("INSERT INTO public.task205_external_restore (id) VALUES (7)")

            subprocess.run(
                [
                    "pg_dump",
                    "--format=custom",
                    "--compress=6",
                    "--no-owner",
                    "--no-privileges",
                    f"--file={dump_path}",
                    self.database_url,
                ],
                check=True,
                capture_output=True,
                text=True,
            )

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("DROP TABLE public.task205_external_restore")
                    cursor.execute("TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_documents")

            snapshot.import_snapshot(self.database_url, dump_path, confirmed=True)

            restored = status.inspect_database(self.database_url)
            self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT to_regclass('public.task205_external_restore') IS NOT NULL"
                )
                public_table_present = bool(cursor.fetchone()[0])

            self.assertFalse(public_table_present)


def _bootstrap_database(connection) -> None:
    init_sql = _sanitized_sql(SCHEMA_DIR / "init.sql")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def _seed_snapshot_fixture(connection) -> None:
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
                    'docs:snapshot-db#0',
                    'docs',
                    'plan',
                    '.agent/plans/agentic/task-plans/task-205.md',
                    'snapshot plan',
                    '[]'::jsonb,
                    0,
                    'snapshot content',
                    'snapshot content',
                    'snapshot-doc-hash'
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
                    'sync-state:docs:repo:DripDropz/daedalus:snapshot-db',
                    'docs',
                    'repo:DripDropz/daedalus:snapshot-db',
                    'cafebabe',
                    %s,
                    %s,
                    '{"repo": "DripDropz/daedalus"}'::jsonb
                )
                """,
                (
                    datetime(2026, 3, 28, 23, 20, tzinfo=timezone.utc),
                    datetime(2026, 3, 28, 23, 21, tzinfo=timezone.utc),
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
