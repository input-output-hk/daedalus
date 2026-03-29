from __future__ import annotations

import io
import json
import os
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import search


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class SearchCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.doc_id = f"docs:{self._testMethodName}#0"
        self.doc_id_secondary = f"docs:{self._testMethodName}#1"
        self.doc_path = f".agent/workflows/{self._testMethodName}.md"
        self.doc_path_secondary = f".agent/workflows/{self._testMethodName}-secondary.md"
        self.project_id = f"project:{self._testMethodName}"
        self.project_node_id = f"PVTI_{self._testMethodName}"
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        self._truncate_tables()
        self._seed_rows()

    def _truncate_tables(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute("TRUNCATE TABLE agentic.kb_project_items, agentic.kb_documents")

    def _seed_rows(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_documents (
                        id, source_domain, doc_kind, source_path, title, heading_path,
                        chunk_index, content, preview_text, content_hash, embedding
                    ) VALUES (
                        %s, 'docs', 'workflow', %s, 'cli workflow',
                        '[]'::jsonb, 0, 'mithril bootstrap workflow details',
                        'mithril bootstrap workflow details', 'docs-cli-hash', %s::vector
                    )
                    """,
                    (self.doc_id, self.doc_path, _vector_literal(_unit_vector(1))),
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_documents (
                        id, source_domain, doc_kind, source_path, title, heading_path,
                        chunk_index, content, preview_text, content_hash, embedding
                    ) VALUES (
                        %s, 'docs', 'workflow', %s, 'cli workflow duplicate',
                        '[]'::jsonb, 0, 'mithril bootstrap task',
                        'mithril bootstrap task', 'docs-cli-hash-secondary', %s::vector
                    )
                    """,
                    (self.doc_id_secondary, self.doc_path_secondary, _vector_literal(_unit_vector(2))),
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_project_items (
                        id, project_owner, project_number, project_item_node_id, content_type,
                        content_id, content_node_id, title, body_text, repo, status, priority,
                        size, work_type, area, phase, kb_impact, field_values, html_url,
                        source_updated_at, embedding, metadata
                    ) VALUES (
                        %s, 'DripDropz', 5, %s, 'issue', NULL, NULL,
                        'mithril bootstrap task', 'mithril bootstrap follow-up work',
                        'DripDropz/daedalus', 'In Progress', 'P1', 'M', 'feature', 'agentic',
                        'phase-5', 'high', '{}'::jsonb, 'https://example.com/project/cli-1',
                        '2026-03-29T00:00:00Z'::timestamptz, %s::vector, '{}'::jsonb
                    )
                    """,
                    (self.project_id, self.project_node_id, _vector_literal(_unit_vector(0))),
                )

    def test_run_search_json_returns_deterministic_hits(self):
        args = Namespace(
            query="mithril bootstrap task",
            mode="bm25",
            limit=2,
            entity_types=["documents"],
            filters=[],
            json=True,
        )

        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch_env(DATABASE_URL=self.database_url):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = search.run_search(args)

        payload = json.loads(stdout.getvalue())
        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        self.assertEqual([hit["id"] for hit in payload["hits"]], [self.doc_id_secondary, self.doc_id])
        self.assertEqual(payload["mode"], "bm25")

    def test_run_search_text_supports_entity_type_filter(self):
        args = Namespace(
            query="mithril bootstrap",
            mode="bm25",
            limit=2,
            entity_types=["project_items"],
            filters=[],
            json=False,
        )

        stdout = io.StringIO()
        stderr = io.StringIO()
        with patch_env(DATABASE_URL=self.database_url):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                exit_code = search.run_search(args)

        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        self.assertIn(self.project_id, stdout.getvalue())
        self.assertNotIn(self.doc_id, stdout.getvalue())


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


def _unit_vector(index: int, *, dimensions: int = 384) -> list[float]:
    return [1.0 if position == index else 0.0 for position in range(dimensions)]


def _vector_literal(values: list[float]) -> str:
    return "[" + ",".join(format(value, ".17g") for value in values) + "]"


if __name__ == "__main__":
    unittest.main()
