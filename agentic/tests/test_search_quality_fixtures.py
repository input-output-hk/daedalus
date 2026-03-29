from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from search_quality_helpers import (
    FIXTURE_FILE_PATH,
    SearchFixtureValidationError,
    load_search_quality_fixtures,
)


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


if __name__ == "__main__":
    unittest.main()
