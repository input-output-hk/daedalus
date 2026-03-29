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
class McpSearchServerDbTests(unittest.TestCase):
    _bootstrapped = False

    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        if not self.__class__._bootstrapped:
            _bootstrap_database(self.connection)
            self.__class__._bootstrapped = True
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
                        chunk_index, content, preview_text, content_hash
                    ) VALUES (
                        'docs:mcp#seed', 'docs', 'workflow', '.agent/workflows/agentic-kb.md',
                        'mithril bootstrap workflow', '[]'::jsonb, 0,
                        'mithril bootstrap workflow details and operational steps',
                        'mithril bootstrap workflow details', 'docs-mcp-seed'
                    ), (
                        'docs:mcp#related', 'docs', 'workflow', '.agent/workflows/related.md',
                        'related workflow', '[]'::jsonb, 0,
                        'workflow details related to mithril bootstrap and sync',
                        'workflow details related to mithril bootstrap and sync', 'docs-mcp-related'
                    )
                    """
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_sync_state (
                        id, source_name, scope_key, repo_commit_hash, last_attempted_at, last_succeeded_at, metadata
                    ) VALUES (
                        'sync-state:docs:mcp', 'docs', 'repo:DripDropz/daedalus:mcp', 'deadbeef',
                        '2026-03-29T00:00:00Z'::timestamptz, '2026-03-29T00:01:00Z'::timestamptz,
                        '{}'::jsonb
                    )
                    """
                )

    def test_tools_list_exposes_only_planned_read_only_tools(self):
        payload = _tools_list(self.server)

        self.assertEqual(
            [tool["name"] for tool in payload["tools"]],
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

    def test_search_and_search_docs_return_seeded_results(self):
        search_payload = _tool_call(self.server, "search", {"query_text": "mithril bootstrap", "mode": "bm25"})
        docs_payload = _tool_call(self.server, "search_docs", {"query_text": "mithril bootstrap", "mode": "bm25"})

        self.assertTrue(any(hit["id"] == "docs:mcp#seed" for hit in search_payload["structuredContent"]["hits"]))
        self.assertEqual(docs_payload["structuredContent"]["entity_types"], ["documents"])
        self.assertTrue(all(hit["entity_type"] == "documents" for hit in docs_payload["structuredContent"]["hits"]))

    def test_get_entity_and_kb_status_return_real_db_backed_payloads(self):
        entity_payload = _tool_call(self.server, "get_entity", {"entity_type": "documents", "id": "docs:mcp#seed"})
        status_payload = _tool_call(self.server, "kb_status", {})

        self.assertEqual(entity_payload["structuredContent"]["row"]["title"], "mithril bootstrap workflow")
        self.assertIn("database_items", status_payload["structuredContent"])
        self.assertIn("freshness", status_payload["structuredContent"])
        self.assertTrue(any(item["name"] == "rows agentic.kb_documents" for item in status_payload["structuredContent"]["database_items"]))

    def test_find_related_returns_bounded_hits_without_the_seed_entity(self):
        payload = _tool_call(self.server, "find_related", {"entity_type": "documents", "id": "docs:mcp#seed", "limit": 2})

        hit_ids = [hit["id"] for hit in payload["structuredContent"]["hits"]]
        self.assertNotIn("docs:mcp#seed", hit_ids)
        self.assertLessEqual(len(hit_ids), 2)
        self.assertTrue(hit_ids)


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
