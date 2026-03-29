from __future__ import annotations

import io
import json
import os
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import entity


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class EntityCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.doc_id = f"docs:{self._testMethodName}#0"
        self.doc_path = f".agent/workflows/{self._testMethodName}.md"
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        self._truncate_tables()
        self._seed_rows()

    def _truncate_tables(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute("TRUNCATE TABLE agentic.kb_documents")

    def _seed_rows(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_documents (
                        id, source_domain, doc_kind, source_path, title, heading_path,
                        chunk_index, content, preview_text, content_hash
                    ) VALUES (
                        %s, 'docs', 'workflow', %s,
                        'entity workflow', '[]'::jsonb, 0, 'full entity body',
                        'full entity body', 'entity-doc-hash'
                    )
                    """,
                    (self.doc_id, self.doc_path),
                )

    def test_run_entity_get_json_returns_full_row(self):
        args = Namespace(entity_type="documents", id=self.doc_id, json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch_env(DATABASE_URL=self.database_url):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = entity.run_entity_get(args)

        payload = json.loads(stdout.getvalue())
        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        self.assertEqual(payload["entity_type"], "documents")
        self.assertEqual(payload["id"], self.doc_id)
        self.assertEqual(payload["row"]["content"], "full entity body")

    def test_run_entity_get_not_found_returns_exit_code_four_and_no_stdout(self):
        args = Namespace(entity_type="documents", id="docs:missing", json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch_env(DATABASE_URL=self.database_url):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = entity.run_entity_get(args)

        self.assertEqual(exit_code, 4)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("documents entity not found for id 'docs:missing'", stderr.getvalue())


class patch_env:
    def __init__(self, **values):
        self.values = values
        self.originals: dict[str, str | None] = {}

    def __enter__(self):
        for key, value in self.values.items():
            self.originals[key] = os.environ.get(key)
            os.environ[key] = value

    def __exit__(self, exc_type, exc, tb):
        for key, original in self.originals.items():
            if original is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = original
        return False


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


if __name__ == "__main__":
    unittest.main()
