from __future__ import annotations

import os
import unittest
from importlib.util import find_spec

from agentic_kb.search import PostgresSearchStore, SearchMode, SearchRequest


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"


class _FakeEmbeddingProvider:
    def __init__(self, embedding: list[float]):
        self.embedding = embedding
        self.calls: list[str] = []

    def embed_text(self, text: str) -> list[float]:
        self.calls.append(text)
        return list(self.embedding)


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class SearchQueryDbTests(unittest.TestCase):
    def setUp(self):
        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.embedding = _unit_vector(0)
        self.embedding_provider = _FakeEmbeddingProvider(self.embedding)
        self.store = PostgresSearchStore.from_database_url(
            self.database_url,
            embedding_provider=self.embedding_provider,
        )
        self.addCleanup(self.store.close)
        self._truncate_tables()
        self._seed_rows()

    def _truncate_tables(self):
        with self.store._connection.transaction():
            with self.store._connection.cursor() as cursor:
                cursor.execute("TRUNCATE TABLE agentic.kb_project_items, agentic.kb_documents")

    def _seed_rows(self):
        with self.store._connection.transaction():
            with self.store._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_documents (
                        id,
                        source_domain,
                        doc_kind,
                        source_path,
                        title,
                        heading_path,
                        chunk_index,
                        content,
                        preview_text,
                        content_hash,
                        embedding
                    ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        '[]'::jsonb,
                        0,
                        %s,
                        %s,
                        %s,
                        %s::vector
                    )
                    """,
                    [
                        (
                            "docs:plan#0",
                            "docs",
                            "plan",
                            ".agent/plans/task-502.md",
                            "search query plan",
                            "hybrid search with rrf",
                            "hybrid search with rrf",
                            "docs-plan-hash",
                            _vector_literal(_unit_vector(1)),
                        ),
                        (
                            "docs:guide#0",
                            "docs",
                            "workflow",
                            ".agent/workflows/agentic-kb.md",
                            "workflow guide",
                            "agentic workflow details",
                            "agentic workflow details",
                            "docs-guide-hash",
                            _vector_literal(_unit_vector(2)),
                        ),
                    ],
                )
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_project_items (
                        id,
                        project_owner,
                        project_number,
                        project_item_node_id,
                        content_type,
                        content_id,
                        content_node_id,
                        title,
                        body_text,
                        repo,
                        status,
                        priority,
                        size,
                        work_type,
                        area,
                        phase,
                        kb_impact,
                        field_values,
                        html_url,
                        source_updated_at,
                        embedding,
                        metadata
                    ) VALUES (
                        %s,
                        'DripDropz',
                        5,
                        %s,
                        %s,
                        NULL,
                        NULL,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        '{}'::jsonb,
                        %s,
                        '2026-03-28T20:00:00Z'::timestamptz,
                        %s::vector,
                        '{}'::jsonb
                    )
                    """,
                    [
                        (
                            "project:item-1",
                            "PVTI_item_1",
                            "issue",
                            "hybrid search task",
                            "Implement hybrid search queries and ranking quality checks",
                            "DripDropz/daedalus",
                            "In Progress",
                            "P1",
                            "M",
                            "feature",
                            "agentic",
                            "phase-5",
                            "high",
                            "https://example.com/project/1",
                            _vector_literal(_unit_vector(0)),
                        ),
                        (
                            "project:item-2",
                            "PVTI_item_2",
                            "issue",
                            "search workflow followup",
                            "Follow up on workflow updates for search",
                            "DripDropz/daedalus",
                            "Todo",
                            "P2",
                            "L",
                            "docs",
                            "agentic",
                            "phase-5",
                            "medium",
                            "https://example.com/project/2",
                            _vector_literal(_unit_vector(2)),
                        ),
                    ],
                )

    def test_bm25_returns_project_item_hits_and_honors_size_filter(self):
        result = self.store.search(
            SearchRequest(
                query_text="hybrid search",
                mode=SearchMode.BM25,
                filters={"entity_type": "project_items", "size": "M"},
            )
        )
        self.assertEqual([hit.id for hit in result.hits], ["project:item-1"])
        self.assertEqual(result.hits[0].fields["size"], "M")
        self.assertIsNotNone(result.hits[0].bm25_score)

    def test_vector_returns_project_item_hits_with_injected_embedding(self):
        result = self.store.search(
            SearchRequest(
                query_text="hybrid semantic",
                mode=SearchMode.VECTOR,
                filters={"entity_type": "project_items", "size": "M"},
            )
        )
        self.assertEqual([hit.id for hit in result.hits], ["project:item-1"])
        self.assertEqual(self.embedding_provider.calls, ["hybrid semantic"])
        self.assertEqual(result.hits[0].vector_distance, 0.0)

    def test_hybrid_merges_project_items_with_documents_deterministically(self):
        result = self.store.search(
            SearchRequest(
                query_text="hybrid search",
                mode=SearchMode.HYBRID,
                limit=3,
            )
        )
        self.assertEqual(
            [hit.id for hit in result.hits],
            ["project:item-1", "docs:plan#0", "project:item-2"],
        )
        self.assertEqual(result.hits[0].fields["title"], "hybrid search task")
        self.assertIsNotNone(result.hits[0].bm25_rank)
        self.assertIsNotNone(result.hits[0].vector_rank)
        self.assertEqual(result.hits[0].hybrid_rank, 1)

    def test_hybrid_project_filter_keeps_non_matching_project_items_out(self):
        result = self.store.search(
            SearchRequest(
                query_text="search",
                mode=SearchMode.HYBRID,
                filters={"entity_type": "project_items", "size": "M"},
            )
        )
        self.assertEqual([hit.id for hit in result.hits], ["project:item-1"])


def _unit_vector(index: int, *, dimensions: int = 384) -> list[float]:
    return [1.0 if position == index else 0.0 for position in range(dimensions)]


def _vector_literal(values: list[float]) -> str:
    return "[" + ",".join(format(value, ".17g") for value in values) + "]"


if __name__ == "__main__":
    unittest.main()
