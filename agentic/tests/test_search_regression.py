from __future__ import annotations

import io
import json
import os
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from importlib.util import find_spec

from agentic_kb.commands import search
from agentic_kb.search import PostgresSearchStore, SearchMode

from search_quality_helpers import (
    assert_fixture_result,
    bootstrap_database,
    fixture_query_embedding,
    load_search_quality_fixtures,
    reset_and_seed_search_quality_corpus,
)


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class SearchRegressionDbTests(unittest.TestCase):
    """Authoritative fixture-backed ranking regression suite."""

    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        bootstrap_database(self.connection)
        reset_and_seed_search_quality_corpus(self.connection)
        self.store = PostgresSearchStore(self.connection)
        self.fixtures = load_search_quality_fixtures()

    def test_canonical_fixtures_execute_against_seeded_corpus(self):
        for fixture in self.fixtures:
            with self.subTest(fixture=fixture.fixture_id, mode=fixture.mode.value):
                result = self.store.search(
                    fixture.build_request(),
                    query_embedding=fixture_query_embedding(fixture),
                )
                assert_fixture_result(self, fixture, result)

    def test_bm25_fixtures_match_search_cli_json_output(self):
        bm25_fixtures = [fixture for fixture in self.fixtures if fixture.mode == SearchMode.BM25]
        self.assertGreater(len(bm25_fixtures), 0, "expected at least one bm25 fixture")

        for fixture in bm25_fixtures:
            with self.subTest(fixture=fixture.fixture_id):
                direct_result = self.store.search(fixture.build_request())
                args = Namespace(
                    query=fixture.query_text,
                    mode=fixture.mode.value,
                    limit=fixture.limit,
                    entity_types=_cli_entity_types(fixture.filters),
                    filters=_cli_filters(fixture.filters),
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
                self.assertEqual(
                    [hit["id"] for hit in payload["hits"]],
                    [hit.id for hit in direct_result.hits],
                )
                self.assertEqual(payload["mode"], "bm25")


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


def _cli_entity_types(filters: dict[str, object]) -> list[str]:
    entity_types = filters.get("entity_type")
    if entity_types is None:
        return []
    if isinstance(entity_types, list):
        return list(entity_types)
    return [str(entity_types)]


def _cli_filters(filters: dict[str, object]) -> list[str]:
    raw_filters: list[str] = []
    for key, value in filters.items():
        if key == "entity_type":
            continue
        raw_filters.append(f"{key}={value}")
    return raw_filters


if __name__ == "__main__":
    unittest.main()
