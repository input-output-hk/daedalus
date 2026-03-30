from __future__ import annotations

import json
import os
import subprocess
import sys
import unittest
from importlib.util import find_spec
from pathlib import Path


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"
ROOT_DIR = Path(__file__).resolve().parents[2]


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class McpSmokeTests(unittest.TestCase):

    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        self._bootstrapped = False
        if not self._bootstrapped:
            _bootstrap_database(self.connection)
            self._bootstrapped = True
        self._seed_rows()
        self.server = _spawn_server(self.database_url)
        self.addCleanup(_stop_server, self.server)
        _initialize_server(self.server)

    def _seed_rows(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute(
                    "TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_project_items, agentic.kb_github_pr_comments, agentic.kb_github_prs, agentic.kb_github_issue_comments, agentic.kb_github_issues, agentic.kb_code_chunks, agentic.kb_documents CASCADE"
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_documents (
                        id, source_domain, doc_kind, source_path, title, heading_path,
                        chunk_index, content, preview_text, content_hash, metadata
                    ) VALUES
                        (
                            'doc:seed-1', 'docs', 'workflow',
                            '.agent/workflows/agentic-kb.md',
                            'mithril bootstrap workflow', '[]'::jsonb, 0,
                            'mithril bootstrap workflow details and operational steps',
                            'mithril bootstrap workflow details', 'doc-seed-1-hash',
                            '{"workflow_description":"bootstrap workflow"}'::jsonb
                        ),
                        (
                            'doc:seed-2', 'docs', 'plan',
                            '.agent/plans/agentic/task-plans/task-303.md',
                            'related workflow plan', '[]'::jsonb, 0,
                            'workflow details related to mithril bootstrap and sync',
                            'workflow details related to mithril bootstrap and sync', 'doc-seed-2-hash',
                            '{"plan_type":"canonical_task_plan","task_id":"task-303"}'::jsonb
                        ),
                        (
                            'doc:seed-3', 'docs', 'sop',
                            '.agent/SOPs/agentic-kb-bootstrap.md',
                            'KB bootstrap standard operating procedure', '[]'::jsonb, 0,
                            'standard operating procedure for bootstrapping the knowledge base',
                            'standard operating procedure for bootstrapping', 'doc-seed-3-hash',
                            '{"sop_type":"bootstrap"}'::jsonb
                        )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_code_chunks (
                        id, repo_path, language, symbol_name, symbol_kind,
                        parent_symbol_name, parent_symbol_kind, chunk_index,
                        start_line, end_line, content, preview_text, content_hash, metadata
                    ) VALUES
                        (
                            'code:seed-1', 'agentic/src/agentic_kb/cli.py', 'python',
                            'mcp_search', 'function', NULL, NULL, 0, 10, 25,
                            'def mcp_search(args):\n    \"\"\"MCP search entrypoint.\"\"\"\n    return serve_mcp_search(args)',
                            'def mcp_search(args): MCP search entrypoint', 'code-seed-1-hash',
                            '{"repo":"DripDropz/daedalus"}'::jsonb
                        ),
                        (
                            'code:seed-2', 'agentic/src/agentic_kb/mcp/search_server.py', 'python',
                            'McpSearchServer', 'class', NULL, NULL, 0, 1, 50,
                            'class McpSearchServer:\n    def __init__(self, db_url):\n        self.db_url = db_url',
                            'class McpSearchServer: init with db_url', 'code-seed-2-hash',
                            '{"repo":"DripDropz/daedalus"}'::jsonb
                        ),
                        (
                            'code:seed-3', 'agentic/src/agentic_kb/search.py', 'python',
                            'bm25_search', 'function', NULL, NULL, 0, 100, 120,
                            'def bm25_search(query_text, limit=20):\n    return search_index.bm25(query_text, limit)',
                            'def bm25_search: BM25 search implementation', 'code-seed-3-hash',
                            '{"repo":"DripDropz/daedalus"}'::jsonb
                        )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_github_issues (
                        id, repo, issue_number, github_node_id, title, state,
                        author_login, labels, body_text, preview_text, html_url,
                        source_created_at, source_updated_at, source_closed_at, metadata
                    ) VALUES
                        (
                            'issue:seed-1', 'DripDropz/daedalus', 101, 'MI_I_kwD',
                            'MCP server returns empty results for valid queries',
                            'open', 'westbam',
                            '["bug","mcp-search"]'::jsonb,
                            'When querying the MCP search endpoint with valid BM25 queries, the server returns an empty hits array even when matching documents exist in the KB.',
                            'MCP server returns empty results for valid queries',
                            'https://github.com/DripDropz/daedalus/issues/101',
                            '2026-03-15T10:00:00Z'::timestamptz,
                            '2026-03-15T10:00:00Z'::timestamptz,
                            NULL,
                            '{}'::jsonb
                        ),
                        (
                            'issue:seed-2', 'DripDropz/daedalus', 202, 'MI_I_kwD',
                            'Add hybrid search support combining BM25 and vector embeddings',
                            'open', 'daedalus-dev',
                            '["enhancement","search"]'::jsonb,
                            'Implement hybrid search that combines BM25 keyword matching with vector embedding similarity for improved recall on technical queries.',
                            'Add hybrid search support combining BM25 and vector embeddings',
                            'https://github.com/DripDropz/daedalus/issues/202',
                            '2026-03-20T14:30:00Z'::timestamptz,
                            '2026-03-20T14:30:00Z'::timestamptz,
                            NULL,
                            '{}'::jsonb
                        )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_github_prs (
                        id, repo, pr_number, github_node_id, title, state,
                        author_login, base_branch, head_branch, labels,
                        body_text, preview_text, html_url,
                        source_created_at, source_updated_at, source_closed_at, source_merged_at, metadata
                    ) VALUES
                        (
                            'pr:seed-1', 'DripDropz/daedalus', 501, 'MI_PRyD',
                            'feat: implement MCP search server with 7 read-only tools',
                            'merged', 'westbam', 'main', 'feat/mcp-search-server',
                            '["feature","mcp"]'::jsonb,
                            'Implements a read-only MCP search server exposing 7 tools: search, search_docs, search_code, search_github, get_entity, find_related, and kb_status.',
                            'feat: implement MCP search server with 7 read-only tools',
                            'https://github.com/DripDropz/daedalus/pull/501',
                            '2026-03-10T09:00:00Z'::timestamptz,
                            '2026-03-10T09:00:00Z'::timestamptz,
                            NULL,
                            '2026-03-12T16:45:00Z'::timestamptz,
                            '{}'::jsonb
                        ),
                        (
                            'pr:seed-2', 'DripDropz/daedalus', 502, 'MI_PRyD',
                            'fix: correct Content-Length header parsing in stdio framing',
                            'merged', 'westbam', 'main', 'fix/stdio-framing',
                            '["bug","mcp"]'::jsonb,
                            'Fixes Content-Length header parsing to correctly handle the \\r\\n delimiter and support streaming JSON-RPC responses.',
                            'fix: correct Content-Length header parsing in stdio framing',
                            'https://github.com/DripDropz/daedalus/pull/502',
                            '2026-03-18T11:00:00Z'::timestamptz,
                            '2026-03-18T11:00:00Z'::timestamptz,
                            NULL,
                            '2026-03-19T09:30:00Z'::timestamptz,
                            '{}'::jsonb
                        )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_project_items (
                        id, project_owner, project_number, project_item_node_id,
                        content_type, content_id, content_node_id, title, body_text,
                        repo, status, priority, field_values, html_url,
                        source_updated_at, metadata
                    ) VALUES
                        (
                            'project:seed-1', 'DripDropzz', 1, 'PI_kwDO',
                            'issue', 'issue:seed-1', 'MI_I_kwD',
                            'MCP server returns empty results for valid queries',
                            'When querying the MCP search endpoint with valid BM25 queries, the server returns an empty hits array.',
                            'DripDropz/daedalus', 'In Progress', 'High',
                            '{"labels":["bug","mcp-search"]}'::jsonb,
                            'https://github.com/DripDropz/daedalus/projects/1#issue-101',
                            '2026-03-15T10:00:00Z'::timestamptz,
                            '{}'::jsonb
                        ),
                        (
                            'project:seed-2', 'DripDropz', 1, 'PI_kwDO',
                            'pullrequest', 'pr:seed-1', 'MI_PRyD',
                            'feat: implement MCP search server with 7 read-only tools',
                            'Implements a read-only MCP search server exposing 7 tools.',
                            'DripDropz/daedalus', 'Done', 'Medium',
                            '{"labels":["feature","mcp"]}'::jsonb,
                            'https://github.com/DripDropz/daedalus/projects/1#pr-501',
                            '2026-03-12T16:45:00Z'::timestamptz,
                            '{}'::jsonb
                        )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_sync_state (
                        id, source_name, scope_key, repo_commit_hash,
                        last_attempted_at, last_succeeded_at, metadata
                    ) VALUES (
                        'sync-state:docs', 'docs', 'repo:DripDropz/daedalus', 'deadbeef',
                        '2026-03-29T00:00:00Z'::timestamptz, '2026-03-29T00:01:00Z'::timestamptz,
                        '{}'::jsonb
                    ), (
                        'sync-state:github', 'github', 'repo:DripDropz/daedalus', 'deadbeef',
                        '2026-03-29T00:02:00Z'::timestamptz, '2026-03-29T00:03:00Z'::timestamptz,
                        '{}'::jsonb
                    )
                    """
                )

    def test_tools_list(self):
        payload = _tools_list(self.server)
        tool_names = [tool["name"] for tool in payload["tools"]]
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
        for tool in payload["tools"]:
            self.assertIn("name", tool)
            self.assertIn("description", tool)
            self.assertIn("inputSchema", tool)

    def test_search(self):
        payload = _tool_call(self.server, "search", {"query_text": "mithril bootstrap", "mode": "bm25"})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("hits", content)
        self.assertIsInstance(content["hits"], list)
        self.assertGreater(len(content["hits"]), 0)
        for hit in content["hits"]:
            self.assertIn("id", hit)
            self.assertIn("entity_type", hit)
            self.assertIn("score", hit)

    def test_search_docs(self):
        payload = _tool_call(self.server, "search_docs", {"query_text": "workflow", "mode": "bm25"})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("hits", content)
        self.assertIsInstance(content["hits"], list)
        self.assertGreater(len(content["hits"]), 0)
        self.assertTrue(all(hit["entity_type"] == "documents" for hit in content["hits"]))

    def test_search_code(self):
        payload = _tool_call(self.server, "search_code", {"query_text": "search server", "mode": "bm25"})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("hits", content)
        self.assertIsInstance(content["hits"], list)
        self.assertGreater(len(content["hits"]), 0)
        for hit in content["hits"]:
            self.assertEqual(hit["entity_type"], "code_chunks")

    def test_search_github(self):
        payload = _tool_call(self.server, "search_github", {"query_text": "MCP search", "mode": "bm25"})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("hits", content)
        self.assertIsInstance(content["hits"], list)
        self.assertGreater(len(content["hits"]), 0)
        for hit in content["hits"]:
            self.assertIn(hit["entity_type"], ("github_issues", "github_prs"))

    def test_get_entity(self):
        payload = _tool_call(self.server, "get_entity", {"entity_type": "documents", "id": "doc:seed-1"})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("row", content)
        self.assertEqual(content["row"]["id"], "doc:seed-1")
        self.assertEqual(content["row"]["title"], "mithril bootstrap workflow")

    def test_find_related(self):
        payload = _tool_call(
            self.server,
            "find_related",
            {"entity_type": "documents", "id": "doc:seed-1", "limit": 3},
        )
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("hits", content)
        self.assertIsInstance(content["hits"], list)
        self.assertLessEqual(len(content["hits"]), 3)
        hit_ids = [hit["id"] for hit in content["hits"]]
        self.assertNotIn("doc:seed-1", hit_ids)

    def test_kb_status(self):
        payload = _tool_call(self.server, "kb_status", {})
        self.assertIn("structuredContent", payload)
        content = payload["structuredContent"]
        self.assertIn("database_items", content)
        self.assertIn("freshness", content)
        self.assertIsInstance(content["database_items"], list)


def _spawn_server(database_url: str) -> subprocess.Popen:
    env = os.environ.copy()
    env["DATABASE_URL"] = database_url
    env.setdefault("OLLAMA_BASE_URL", "http://127.0.0.1:1")
    env.setdefault("OLLAMA_EMBED_MODEL", "all-minilm")
    env["PYTHONPATH"] = str(ROOT_DIR / "agentic" / "src")
    return subprocess.Popen(
        [sys.executable, "-m", "agentic_kb.cli", "mcp-search"],
        cwd=ROOT_DIR,
        env=env,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def _stop_server(process: subprocess.Popen) -> None:
    if process.poll() is None:
        process.terminate()
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=5)

    for stream_name in ("stdin", "stdout", "stderr"):
        stream = getattr(process, stream_name, None)
        if stream is not None and not stream.closed:
            stream.close()


def _initialize_server(process: subprocess.Popen) -> dict[str, object]:
    return _send_message(
        process,
        {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {"protocolVersion": "2025-03-26", "capabilities": {}, "clientInfo": {"name": "test", "version": "1"}},
        },
    )


def _tools_list(process: subprocess.Popen) -> dict[str, object]:
    response = _send_message(
        process,
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
    )
    return response["result"]


def _tool_call(process: subprocess.Popen, name: str, arguments: dict[str, object]) -> dict[str, object]:
    response = _send_message(
        process,
        {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {"name": name, "arguments": arguments},
        },
    )
    return response["result"]


def _send_message(process: subprocess.Popen, payload: dict[str, object]) -> dict[str, object]:
    encoded = json.dumps(payload, ensure_ascii=True, separators=(",", ":")).encode("utf-8")
    process.stdin.write(f"Content-Length: {len(encoded)}\r\n\r\n".encode("ascii"))
    process.stdin.write(encoded)
    process.stdin.flush()
    return _read_message(process.stdout)


def _read_message(stream) -> dict[str, object]:
    headers: dict[str, str] = {}
    while True:
        line = stream.readline()
        if not line:
            raise AssertionError("MCP server closed stdout unexpectedly")
        if line in (b"\r\n", b"\n"):
            break
        key, value = line.decode("utf-8").split(":", 1)
        headers[key.strip().lower()] = value.strip()

    body = stream.read(int(headers["content-length"]))
    return json.loads(body.decode("utf-8"))


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
