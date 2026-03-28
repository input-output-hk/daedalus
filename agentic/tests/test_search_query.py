from __future__ import annotations

import unittest

from agentic_kb.search import (
    CompiledSql,
    SearchEntityType,
    SearchMode,
    SearchRequest,
    SearchValidationError,
    build_bm25_entity_query,
    build_entity_filter_clause,
    build_vector_entity_query,
    get_search_entity_config,
    merge_hybrid_hits,
    select_search_entity_configs,
)
from agentic_kb.search.query import _RowHit


class SearchRequestTests(unittest.TestCase):
    def test_rejects_empty_query_text(self):
        with self.assertRaisesRegex(SearchValidationError, "query_text"):
            SearchRequest(query_text="   ")

    def test_rejects_non_positive_limit(self):
        with self.assertRaisesRegex(SearchValidationError, "limit"):
            SearchRequest(query_text="mithril", limit=0)

    def test_normalizes_mode_string(self):
        request = SearchRequest(query_text="mithril", mode="bm25")
        self.assertEqual(request.mode, SearchMode.BM25)


class SearchSelectionTests(unittest.TestCase):
    def test_entity_type_filter_selects_requested_configs(self):
        selected = select_search_entity_configs({"entity_type": ["documents", "project_items"]})
        self.assertEqual(
            tuple(config.entity_type for config in selected),
            (SearchEntityType.DOCUMENTS, SearchEntityType.PROJECT_ITEMS),
        )

    def test_repo_path_prefix_filters_scope_to_code_chunks_only(self):
        selected = select_search_entity_configs({"repo_path_prefix": "agentic/src"})
        self.assertEqual(
            tuple(config.entity_type for config in selected),
            (SearchEntityType.CODE_CHUNKS,),
        )

    def test_project_size_filter_selects_project_items_only(self):
        selected = select_search_entity_configs({"size": "M"})
        self.assertEqual(
            tuple(config.entity_type for config in selected),
            (SearchEntityType.PROJECT_ITEMS,),
        )

    def test_rejects_unknown_filter_key(self):
        with self.assertRaisesRegex(SearchValidationError, "Unsupported search filters"):
            select_search_entity_configs({"unknown": "value"})

    def test_rejects_invalid_entity_type_value(self):
        with self.assertRaisesRegex(SearchValidationError, "Unsupported entity_type"):
            select_search_entity_configs({"entity_type": ["documents", "bogus"]})


class SearchSqlBuilderTests(unittest.TestCase):
    def test_prefix_filter_clause_uses_like_prefix(self):
        config = get_search_entity_config(SearchEntityType.DOCUMENTS)
        sql, params = build_entity_filter_clause(
            config,
            {"source_path_prefix": ".agent/plans"},
            table_alias="base",
        )
        self.assertEqual(sql, 'base."source_path" LIKE %s')
        self.assertEqual(params, (".agent/plans%",))

    def test_non_bm25_budget_project_filter_stays_in_where_clause(self):
        config = get_search_entity_config(SearchEntityType.PROJECT_ITEMS)
        compiled = build_bm25_entity_query(
            config,
            "search quality",
            {"size": "M"},
            candidate_limit=25,
            final_limit=10,
        )
        self.assertIsInstance(compiled, CompiledSql)
        self.assertIn('base."size" = %s', compiled.sql)
        self.assertNotIn('"size" @@@ %s', compiled.sql)
        self.assertEqual(compiled.params[-2:], ("M", 10))

    def test_vector_query_uses_same_project_filter_clause(self):
        config = get_search_entity_config(SearchEntityType.PROJECT_ITEMS)
        compiled = build_vector_entity_query(
            config,
            [1.0, 0.0, 0.0],
            {"size": "L"},
            limit=7,
        )
        self.assertIn('base."embedding" IS NOT NULL AND base."size" = %s', compiled.sql)
        self.assertEqual(compiled.params[-2:], ('[1,0,0]', 7))
        self.assertEqual(compiled.params[1], "L")

    def test_rejects_wrong_query_embedding_dimension(self):
        from agentic_kb.search.query import _coerce_query_embedding

        with self.assertRaisesRegex(SearchValidationError, "dimension 384"):
            _coerce_query_embedding([1.0, 0.0], query_text="search", embedding_provider=None)


class HybridMergeTests(unittest.TestCase):
    def test_rrf_deduplicates_and_sums_modality_contributions(self):
        bm25_hits = [
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="docs:1",
                fields={"id": "docs:1"},
                bm25_score=2.0,
            ),
            _RowHit(
                entity_type=SearchEntityType.PROJECT_ITEMS,
                source_domain="project",
                id="proj:1",
                fields={"id": "proj:1"},
                bm25_score=1.0,
            ),
        ]
        vector_hits = [
            _RowHit(
                entity_type=SearchEntityType.PROJECT_ITEMS,
                source_domain="project",
                id="proj:1",
                fields={"id": "proj:1"},
                vector_distance=0.0,
            ),
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="docs:2",
                fields={"id": "docs:2"},
                vector_distance=0.1,
            ),
        ]

        merged = merge_hybrid_hits(bm25_hits, vector_hits, limit=3)
        self.assertEqual([hit.id for hit in merged], ["proj:1", "docs:1", "docs:2"])
        self.assertEqual(merged[0].bm25_rank, 2)
        self.assertEqual(merged[0].vector_rank, 1)
        self.assertGreater(merged[0].fused_score, merged[1].fused_score)

    def test_rrf_tie_breaks_stably_by_best_rank_then_identity(self):
        bm25_hits = [
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="a",
                fields={"id": "a"},
                bm25_score=1.0,
            ),
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="b",
                fields={"id": "b"},
                bm25_score=0.9,
            ),
        ]
        vector_hits = [
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="b",
                fields={"id": "b"},
                vector_distance=0.0,
            ),
            _RowHit(
                entity_type=SearchEntityType.DOCUMENTS,
                source_domain="docs",
                id="a",
                fields={"id": "a"},
                vector_distance=0.1,
            ),
        ]

        merged = merge_hybrid_hits(bm25_hits, vector_hits, limit=2)
        self.assertEqual([hit.id for hit in merged], ["a", "b"])


if __name__ == "__main__":
    unittest.main()
