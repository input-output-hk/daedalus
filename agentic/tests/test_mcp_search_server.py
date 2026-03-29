from __future__ import annotations

import unittest
from unittest.mock import patch

from agentic_kb.commands import entity as entity_command
from agentic_kb.commands import status as status_command
from agentic_kb.embed.client import EmbeddingConnectionError
from agentic_kb.mcp.search_server import (
    DOC_FILTER_KEYS,
    MCP_ENTITY_NOT_FOUND,
    MCP_INVALID_PARAMS,
    SearchMcpServer,
)
from agentic_kb.search import SearchEntityType, SearchHit, SearchMode, SearchResultSet


class SearchMcpServerTests(unittest.TestCase):
    def setUp(self):
        self.server = SearchMcpServer(config=_fake_config())

    def test_list_tools_exposes_only_the_approved_read_only_surface(self):
        tool_names = [tool["name"] for tool in self.server.list_tools()]

        self.assertEqual(
            tool_names,
            [
                "search",
                "search_docs",
                "search_code",
                "search_github",
                "get_entity",
                "find_related",
                "kb_status",
            ],
        )

    @patch("agentic_kb.mcp.search_server.PostgresSearchStore.from_config")
    def test_search_returns_existing_search_serializer_payload(self, from_config):
        store = from_config.return_value.__enter__.return_value
        store.search.return_value = SearchResultSet(
            query_text="mithril",
            mode=SearchMode.HYBRID,
            limit=2,
            filters={"entity_type": ["documents"]},
            entity_types=(SearchEntityType.DOCUMENTS,),
            hits=(
                SearchHit(
                    entity_type=SearchEntityType.DOCUMENTS,
                    source_domain="docs",
                    id="docs:1",
                    fields={"id": "docs:1", "title": "Workflow"},
                    fused_score=0.2,
                    hybrid_rank=1,
                ),
            ),
        )

        result = self.server.call_tool(
            "search",
            {"query_text": "mithril", "mode": "hybrid", "limit": 2, "entity_types": ["documents"]},
        )

        self.assertEqual(result["structuredContent"]["query_text"], "mithril")
        self.assertEqual(result["structuredContent"]["mode"], "hybrid")
        self.assertEqual(result["structuredContent"]["hits"][0]["id"], "docs:1")

    @patch("agentic_kb.mcp.search_server.PostgresSearchStore.from_config")
    def test_search_docs_injects_documents_entity_type(self, from_config):
        store = from_config.return_value.__enter__.return_value
        captured_requests = []
        store.search.side_effect = lambda request: captured_requests.append(request) or SearchResultSet(
            query_text=request.query_text,
            mode=request.mode,
            limit=request.limit,
            filters=request.filters,
            entity_types=(SearchEntityType.DOCUMENTS,),
            hits=(),
        )

        result = self.server.call_tool(
            "search_docs",
            {"query_text": "workflow", "filters": {"doc_kind": "workflow"}},
        )

        self.assertEqual(captured_requests[0].filters["entity_type"], ["documents"])
        self.assertEqual(result["structuredContent"]["entity_types"], ["documents"])
        self.assertEqual(result["structuredContent"]["filters"]["doc_kind"], "workflow")

    def test_search_docs_schema_and_filter_allowlist_include_plan_metadata_filters(self):
        tools = {tool["name"]: tool for tool in self.server.list_tools()}
        docs_filters = tools["search_docs"]["inputSchema"]["properties"]["filters"]["properties"]

        self.assertEqual(
            DOC_FILTER_KEYS,
            ("doc_kind", "source_path_prefix", "task_id", "planning_status", "build_status", "plan_type"),
        )
        self.assertEqual(tuple(docs_filters), DOC_FILTER_KEYS)

    def test_search_github_rejects_incompatible_entity_type_and_filter(self):
        with self.assertRaisesRegex(Exception, "Selected entity_type values do not support the requested filters"):
            self.server.call_tool(
                "search_github",
                {
                    "query_text": "review",
                    "entity_types": ["github_issue_comments"],
                    "filters": {"state": "open"},
                },
            )

    @patch("agentic_kb.mcp.search_server.PostgresSearchStore.from_config")
    def test_search_github_narrows_default_entity_types_from_filters(self, from_config):
        store = from_config.return_value.__enter__.return_value
        captured_requests = []
        store.search.side_effect = lambda request: captured_requests.append(request) or SearchResultSet(
            query_text=request.query_text,
            mode=request.mode,
            limit=request.limit,
            filters=request.filters,
            entity_types=(SearchEntityType.GITHUB_PR_COMMENTS,),
            hits=(),
        )

        result = self.server.call_tool(
            "search_github",
            {"query_text": "review", "filters": {"comment_type": "review_comment"}},
        )

        self.assertEqual(captured_requests[0].filters["entity_type"], ["github_pr_comments"])
        self.assertEqual(result["structuredContent"]["entity_types"], ["github_pr_comments"])

    @patch("agentic_kb.mcp.search_server.get_entity_payload")
    def test_get_entity_returns_existing_entity_serializer_payload(self, get_entity_payload):
        get_entity_payload.return_value = {
            "entity_type": SearchEntityType.DOCUMENTS,
            "id": "docs:1",
            "table_name": "agentic.kb_documents",
            "row": {"id": "docs:1", "title": "Workflow"},
        }

        result = self.server.call_tool("get_entity", {"entity_type": "documents", "id": "docs:1"})

        self.assertEqual(result["structuredContent"]["entity_type"], "documents")
        self.assertEqual(result["structuredContent"]["row"]["title"], "Workflow")

    @patch("agentic_kb.mcp.search_server.get_entity_payload", side_effect=entity_command.EntityCommandError("documents entity not found for id 'docs:404'"))
    def test_get_entity_missing_row_is_an_mcp_not_found_error(self, _get_entity_payload):
        response = self.server.handle_message(
            {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": "get_entity", "arguments": {"entity_type": "documents", "id": "docs:404"}},
            }
        )

        self.assertEqual(response["error"]["code"], MCP_ENTITY_NOT_FOUND)

    @patch("agentic_kb.mcp.search_server.collect_status_report")
    def test_kb_status_returns_degraded_status_as_success_payload(self, collect_status_report):
        collect_status_report.return_value = status_command.StatusReport(
            healthcheck=False,
            ok=False,
            config_items=(status_command.CheckResult(name="DATABASE_URL", ok=True, detail="db"),),
            environment_items=(),
            dependency_items=(status_command.CheckResult(name="ollama api", ok=False, detail="down"),),
            database_items=(),
            embedding_compatibility=status_command.EmbeddingCompatibilityReport(
                state="unavailable",
                detail="embedding compatibility inspection unavailable",
                runtime_contract=None,
                snapshot_contract=None,
                snapshot_source=None,
            ),
            freshness=status_command.FreshnessReport(
                up_to_date=False,
                items=(
                    status_command.FreshnessItem(
                        name="github issues",
                        status="stale",
                        detail="stored watermark lags behind the latest GitHub update",
                        baseline="2026-03-28T12:00:00Z",
                        observed="2026-03-29T12:00:00Z",
                    ),
                ),
            ),
            notes=("degraded",),
        )

        result = self.server.call_tool("kb_status", {})

        self.assertFalse(result["structuredContent"]["ok"])
        self.assertEqual(result["structuredContent"]["dependency_items"][0]["detail"], "down")
        self.assertEqual(result["structuredContent"]["freshness"]["items"][0]["status"], "stale")

    @patch("agentic_kb.mcp.search_server.PostgresSearchStore.from_config")
    @patch("agentic_kb.mcp.search_server.get_entity_payload")
    def test_find_related_uses_bm25_seed_text_and_excludes_seed_entity(self, get_entity_payload, from_config):
        get_entity_payload.return_value = {
            "entity_type": SearchEntityType.DOCUMENTS,
            "id": "docs:seed",
            "table_name": "agentic.kb_documents",
            "row": {
                "id": "docs:seed",
                "title": "Workflow title",
                "preview_text": "Useful preview",
                "content": "Full content",
            },
        }
        store = from_config.return_value.__enter__.return_value
        captured_requests = []
        store.search.side_effect = lambda request: captured_requests.append(request) or SearchResultSet(
            query_text=request.query_text,
            mode=request.mode,
            limit=request.limit,
            filters=request.filters,
            entity_types=(SearchEntityType.DOCUMENTS, SearchEntityType.CODE_CHUNKS),
            hits=(
                SearchHit(
                    entity_type=SearchEntityType.DOCUMENTS,
                    source_domain="docs",
                    id="docs:seed",
                    fields={"id": "docs:seed"},
                    bm25_score=1.0,
                    bm25_rank=1,
                ),
                SearchHit(
                    entity_type=SearchEntityType.CODE_CHUNKS,
                    source_domain="code",
                    id="code:1",
                    fields={"id": "code:1"},
                    bm25_score=0.9,
                    bm25_rank=2,
                ),
            ),
        )

        result = self.server.call_tool("find_related", {"entity_type": "documents", "id": "docs:seed", "limit": 1})

        self.assertEqual(captured_requests[0].mode, SearchMode.BM25)
        self.assertEqual(captured_requests[0].limit, 20)
        self.assertIn("Workflow title", captured_requests[0].query_text)
        self.assertEqual([hit["id"] for hit in result["structuredContent"]["hits"]], ["code:1"])

    def test_find_related_fails_when_seed_has_no_usable_text(self):
        with patch("agentic_kb.mcp.search_server.get_entity_payload") as get_entity_payload:
            get_entity_payload.return_value = {
                "entity_type": SearchEntityType.DOCUMENTS,
                "id": "docs:seed",
                "table_name": "agentic.kb_documents",
                "row": {"id": "docs:seed", "title": " ", "preview_text": "", "content": None},
            }

            response = self.server.handle_message(
                {
                    "jsonrpc": "2.0",
                    "id": 7,
                    "method": "tools/call",
                    "params": {"name": "find_related", "arguments": {"entity_type": "documents", "id": "docs:seed"}},
                }
            )

        self.assertEqual(response["error"]["code"], MCP_INVALID_PARAMS)

    @patch("agentic_kb.mcp.search_server.PostgresSearchStore.from_config")
    def test_vector_mode_errors_propagate_as_tool_failures(self, from_config):
        store = from_config.return_value.__enter__.return_value
        store.search.side_effect = EmbeddingConnectionError("Unable to reach Ollama embed API")

        response = self.server.handle_message(
            {
                "jsonrpc": "2.0",
                "id": 11,
                "method": "tools/call",
                "params": {"name": "search", "arguments": {"query_text": "mithril", "mode": "vector"}},
            }
        )

        self.assertEqual(response["error"]["code"], -32603)
        self.assertIn("Unable to reach Ollama embed API", response["error"]["message"])


def _fake_config():
    return type(
        "Config",
        (),
        {
            "database_url": "postgresql://agentic:agentic@db:5432/agentic_kb",
            "ollama_base_url": "http://ollama:11434",
            "ollama_embed_model": "all-minilm",
            "github_token": None,
        },
    )()


if __name__ == "__main__":
    unittest.main()
