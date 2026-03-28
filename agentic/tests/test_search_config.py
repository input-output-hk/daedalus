from __future__ import annotations

import unittest

from agentic_kb.search import (
    GLOBAL_SEARCH_FILTERS,
    SEARCH_ENTITY_REGISTRY,
    SearchEntityType,
    SearchFilterMatchType,
    SearchFilterScope,
    SearchSourceDomain,
    entity_types_for_source_domain,
    get_search_entity_config,
    list_global_filters,
    list_search_entity_configs,
    list_searchable_tables,
    list_supported_filters,
)


class SearchConfigTests(unittest.TestCase):
    def test_registry_covers_all_current_searchable_entities_exactly_once(self):
        self.assertEqual(
            tuple(SEARCH_ENTITY_REGISTRY),
            (
                SearchEntityType.DOCUMENTS,
                SearchEntityType.CODE_CHUNKS,
                SearchEntityType.GITHUB_ISSUES,
                SearchEntityType.GITHUB_ISSUE_COMMENTS,
                SearchEntityType.GITHUB_PRS,
                SearchEntityType.GITHUB_PR_COMMENTS,
                SearchEntityType.PROJECT_ITEMS,
            ),
        )
        self.assertEqual(
            list_searchable_tables(),
            (
                "agentic.kb_documents",
                "agentic.kb_code_chunks",
                "agentic.kb_github_issues",
                "agentic.kb_github_issue_comments",
                "agentic.kb_github_prs",
                "agentic.kb_github_pr_comments",
                "agentic.kb_project_items",
            ),
        )

    def test_registry_entity_names_and_table_names_are_unique(self):
        configs = list_search_entity_configs()
        self.assertEqual(len(configs), 7)
        self.assertEqual(len({config.entity_type for config in configs}), 7)
        self.assertEqual(len({config.table_name for config in configs}), 7)

    def test_global_filters_are_registry_owned_not_column_backed(self):
        self.assertEqual(list_global_filters(), GLOBAL_SEARCH_FILTERS)
        self.assertEqual([filter_config.key for filter_config in GLOBAL_SEARCH_FILTERS], ["entity_type"])
        self.assertTrue(
            all(filter_config.scope == SearchFilterScope.GLOBAL for filter_config in GLOBAL_SEARCH_FILTERS)
        )
        self.assertTrue(all(filter_config.column_name is None for filter_config in GLOBAL_SEARCH_FILTERS))

    def test_entity_filters_map_to_expected_columns_and_prefix_semantics(self):
        documents = get_search_entity_config(SearchEntityType.DOCUMENTS)
        docs_filters = {filter_config.key: filter_config for filter_config in documents.filters}
        self.assertEqual(docs_filters["doc_kind"].column_name, "doc_kind")
        self.assertEqual(docs_filters["source_path_prefix"].column_name, "source_path")
        self.assertEqual(
            docs_filters["source_path_prefix"].match_type,
            SearchFilterMatchType.PREFIX,
        )

        code_chunks = get_search_entity_config(SearchEntityType.CODE_CHUNKS)
        code_filters = {filter_config.key: filter_config for filter_config in code_chunks.filters}
        self.assertEqual(code_filters["repo_path_prefix"].column_name, "repo_path")
        self.assertEqual(
            code_filters["repo_path_prefix"].match_type,
            SearchFilterMatchType.PREFIX,
        )
        self.assertEqual(code_filters["language"].column_name, "language")
        self.assertEqual(code_filters["symbol_kind"].column_name, "symbol_kind")

    def test_project_item_filters_include_current_typed_columns(self):
        config = get_search_entity_config(SearchEntityType.PROJECT_ITEMS)
        filter_columns = {filter_config.key: filter_config.column_name for filter_config in config.filters}
        self.assertEqual(
            filter_columns,
            {
                "repo": "repo",
                "status": "status",
                "priority": "priority",
                "size": "size",
                "work_type": "work_type",
                "area": "area",
                "phase": "phase",
                "kb_impact": "kb_impact",
                "content_type": "content_type",
            },
        )

    def test_bm25_columns_match_current_index_contracts(self):
        self.assertEqual(
            get_search_entity_config(SearchEntityType.DOCUMENTS).bm25_columns,
            (
                "id",
                "source_domain",
                "doc_kind",
                "source_path",
                "title",
                "preview_text",
                "content",
            ),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.CODE_CHUNKS).bm25_columns,
            (
                "id",
                "repo_path",
                "language",
                "symbol_name",
                "symbol_kind",
                "preview_text",
                "content",
            ),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.GITHUB_ISSUES).bm25_columns,
            ("id", "repo", "issue_number", "state", "title", "preview_text", "body_text"),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.GITHUB_ISSUE_COMMENTS).bm25_columns,
            ("id", "repo", "issue_number", "preview_text", "body_text"),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.GITHUB_PRS).bm25_columns,
            ("id", "repo", "pr_number", "state", "title", "preview_text", "body_text"),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.GITHUB_PR_COMMENTS).bm25_columns,
            ("id", "repo", "pr_number", "comment_type", "repo_path", "preview_text", "body_text"),
        )
        self.assertEqual(
            get_search_entity_config(SearchEntityType.PROJECT_ITEMS).bm25_columns,
            (
                "id",
                "project_owner",
                "project_number",
                "content_type",
                "repo",
                "status",
                "priority",
                "work_type",
                "area",
                "phase",
                "kb_impact",
                "title",
                "body_text",
            ),
        )

    def test_supported_filters_include_global_plus_entity_specific_entries(self):
        filter_keys = [
            filter_config.key
            for filter_config in list_supported_filters(SearchEntityType.GITHUB_PRS)
        ]
        self.assertEqual(filter_keys, ["entity_type", "repo", "state"])
        self.assertEqual(
            list_supported_filters(),
            GLOBAL_SEARCH_FILTERS,
        )

    def test_source_domain_groupings_match_expected_entities(self):
        self.assertEqual(
            entity_types_for_source_domain(SearchSourceDomain.DOCS),
            (SearchEntityType.DOCUMENTS,),
        )
        self.assertEqual(
            entity_types_for_source_domain(SearchSourceDomain.CODE),
            (SearchEntityType.CODE_CHUNKS,),
        )
        self.assertEqual(
            entity_types_for_source_domain(SearchSourceDomain.GITHUB),
            (
                SearchEntityType.GITHUB_ISSUES,
                SearchEntityType.GITHUB_ISSUE_COMMENTS,
                SearchEntityType.GITHUB_PRS,
                SearchEntityType.GITHUB_PR_COMMENTS,
            ),
        )
        self.assertEqual(
            entity_types_for_source_domain(SearchSourceDomain.PROJECT),
            (SearchEntityType.PROJECT_ITEMS,),
        )

    def test_search_package_exports_registry_surface(self):
        import agentic_kb.search as search

        self.assertIs(search.SEARCH_ENTITY_REGISTRY, SEARCH_ENTITY_REGISTRY)
        self.assertIs(search.get_search_entity_config, get_search_entity_config)
        self.assertIs(search.list_supported_filters, list_supported_filters)


if __name__ == "__main__":
    unittest.main()
