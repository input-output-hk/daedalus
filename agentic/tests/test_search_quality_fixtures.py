from __future__ import annotations

import io
import json
import os
import tempfile
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import search
from agentic_kb.search import PostgresSearchStore

from search_quality_helpers import (
    FIXTURE_FILE_PATH,
    SearchFixtureValidationError,
    assert_fixture_result,
    bootstrap_database,
    fixture_query_embedding,
    load_search_quality_fixtures,
    reset_and_seed_search_quality_corpus,
)


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"


class SearchQualityFixtureLoaderTests(unittest.TestCase):
    def test_loads_canonical_fixture_file(self):
        fixtures = load_search_quality_fixtures()

        self.assertEqual(FIXTURE_FILE_PATH.name, "search-fixtures.yaml")
        self.assertEqual([fixture.fixture_id for fixture in fixtures], [
            "bm25_documents_task_plan",
            "vector_code_fixture_loader",
            "hybrid_cross_entity_search_quality",
        ])

    def test_rejects_duplicate_fixture_ids(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "duplicate search fixture id"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: duplicate
                    query_text: search quality
                    mode: bm25
                    expectations:
                      top_hit: docs:1
                  - id: duplicate
                    query_text: search quality
                    mode: bm25
                    expectations:
                      top_hit: docs:2
                """
            )

    def test_rejects_invalid_mode(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "unsupported mode"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: invalid-mode
                    query_text: search quality
                    mode: lexical
                    expectations:
                      top_hit: docs:1
                """
            )

    def test_rejects_empty_query_text(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "non-empty query_text"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: empty-query
                    query_text: "   "
                    mode: bm25
                    expectations:
                      top_hit: docs:1
                """
            )

    def test_rejects_unsupported_expectation_keys(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "unsupported expectation keys"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: bad-expectation
                    query_text: search quality
                    mode: bm25
                    expectations:
                      exact_hits:
                        - docs:1
                """
            )

    def test_rejects_vector_fixture_without_query_embedding_key(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "must set query_embedding_key"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: missing-query-embedding
                    query_text: semantic fixture
                    mode: vector
                    expectations:
                      top_hit: code:1
                """
            )

    def test_rejects_unknown_registry_filter(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "current search registry"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: bad-filter
                    query_text: search quality
                    mode: bm25
                    filters:
                      unknown_filter: value
                    expectations:
                      top_hit: docs:1
                """
            )

    def test_rejects_bm25_fixture_with_query_embedding_key(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "must not set query_embedding_key"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: bm25-with-embedding
                    query_text: search quality
                    mode: bm25
                    query_embedding_key: code_fixture_loader
                    expectations:
                      top_hit: docs:1
                """
            )

    def test_rejects_unknown_query_embedding_key(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, "unknown query_embedding_key"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: unknown-query-embedding
                    query_text: semantic fixture
                    mode: hybrid
                    query_embedding_key: missing_key
                    expectations:
                      top_hit: docs:1
                """
            )

    def test_rejects_contradictory_top_hit_and_ordered_prefix(self):
        with self.assertRaisesRegex(SearchFixtureValidationError, r"top_hit must match ordered_prefix\[0\]"):
            self._load_fixture_text(
                """
                version: 1
                fixtures:
                  - id: contradictory
                    query_text: search quality
                    mode: bm25
                    expectations:
                      top_hit: docs:1
                      ordered_prefix:
                        - docs:2
                """
            )

    def _load_fixture_text(self, fixture_text: str):
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture_path = Path(temp_dir) / "fixtures.yaml"
            fixture_path.write_text(fixture_text, encoding="utf-8")
            return load_search_quality_fixtures(fixture_path)


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class SearchQualityFixtureDbTests(unittest.TestCase):
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
            with self.subTest(fixture=fixture.fixture_id):
                result = self.store.search(
                    fixture.build_request(),
                    query_embedding=fixture_query_embedding(fixture),
                )
                assert_fixture_result(self, fixture, result)

    def test_bm25_fixture_matches_search_cli_json_output(self):
        fixture = next(fixture for fixture in self.fixtures if fixture.mode.value == "bm25")
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
        self.assertEqual([hit["id"] for hit in payload["hits"]], [hit.id for hit in direct_result.hits])
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
