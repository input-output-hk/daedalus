from __future__ import annotations

import io
import json
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from unittest.mock import patch

from agentic_kb.commands import search
from agentic_kb.search import SearchEntityType, SearchHit, SearchMode, SearchRequest, SearchResultSet


class SearchCommandTests(unittest.TestCase):
    def test_build_search_request_from_args_supports_entity_types_and_filters(self):
        args = Namespace(
            query="mithril bootstrap",
            mode="bm25",
            limit=5,
            entity_types=["documents", "project_items"],
            filters=["doc_kind=workflow", "repo=DripDropz/daedalus"],
        )

        request = search.build_search_request_from_args(args)

        self.assertEqual(request, SearchRequest(
            query_text="mithril bootstrap",
            mode=SearchMode.BM25,
            limit=5,
            filters={
                "entity_type": ["documents", "project_items"],
                "doc_kind": "workflow",
                "repo": "DripDropz/daedalus",
            },
        ))

    def test_parse_cli_filters_rejects_duplicate_filter_keys(self):
        with self.assertRaisesRegex(Exception, "Duplicate search filter key"):
            search.parse_cli_filters([], ["repo=one", "repo=two"])

    def test_parse_filter_argument_requires_key_value_syntax(self):
        with self.assertRaisesRegex(Exception, "key=value"):
            search.parse_filter_argument("repo")

    def test_serialize_search_result_set_is_deterministic_json_shape(self):
        result = SearchResultSet(
            query_text="mithril",
            mode=SearchMode.HYBRID,
            limit=2,
            filters={"entity_type": ["documents"]},
            entity_types=(),
            hits=(
                SearchHit(
                    entity_type=SearchEntityType.DOCUMENTS,
                    source_domain="docs",
                    id="docs:1",
                    fields={"id": "docs:1", "title": "Doc 1"},
                    fused_score=0.5,
                    hybrid_rank=1,
                ),
            ),
        )

        payload = search.serialize_search_result_set(result)

        self.assertEqual(list(payload.keys()), ["query_text", "mode", "limit", "filters", "entity_types", "hits"])
        self.assertEqual(payload["hits"][0]["id"], "docs:1")

    def test_print_search_result_text_includes_title_location_and_preview(self):
        result = SearchResultSet(
            query_text="mithril",
            mode=SearchMode.BM25,
            limit=1,
            filters={},
            entity_types=(),
            hits=(
                SearchHit(
                    entity_type=SearchEntityType.DOCUMENTS,
                    source_domain="docs",
                    id="docs:1",
                    fields={
                        "id": "docs:1",
                        "title": "Workflow",
                        "source_path": ".agent/workflows/agentic-kb.md",
                        "preview_text": "Search workflow details",
                    },
                ),
            ),
        )

        stdout = io.StringIO()
        with redirect_stdout(stdout):
            search.print_search_result_text(result)

        output = stdout.getvalue()
        self.assertIn("search bm25", output)
        self.assertIn("Workflow", output)
        self.assertIn(".agent/workflows/agentic-kb.md", output)
        self.assertIn("Search workflow details", output)

    @patch("agentic_kb.commands.search.PostgresSearchStore.from_config")
    @patch("agentic_kb.commands.search.AgenticConfig.from_env")
    def test_run_search_json_emits_only_json_on_success(self, from_env, from_config):
        from_env.return_value = object()
        result = SearchResultSet(
            query_text="mithril",
            mode=SearchMode.HYBRID,
            limit=1,
            filters={},
            entity_types=(),
            hits=(),
        )
        store = from_config.return_value.__enter__.return_value
        store.search.return_value = result
        args = Namespace(query="mithril", mode="hybrid", limit=10, entity_types=[], filters=[], json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            exit_code = search.run_search(args)

        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        self.assertEqual(json.loads(stdout.getvalue()), {
            "query_text": "mithril",
            "mode": "hybrid",
            "limit": 1,
            "filters": {},
            "entity_types": [],
            "hits": [],
        })

    @patch("agentic_kb.commands.search.PostgresSearchStore.from_config", side_effect=Exception("db down"))
    @patch("agentic_kb.commands.search.AgenticConfig.from_env")
    def test_run_search_writes_errors_to_stderr_only(self, from_env, _from_config):
        from_env.return_value = object()
        args = Namespace(query="mithril", mode="hybrid", limit=10, entity_types=[], filters=[], json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            exit_code = search.run_search(args)

        self.assertEqual(exit_code, 1)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("search failed: db down", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
