from __future__ import annotations

import unittest

from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import docs


class FakeEmbeddingClient:
    def embed_texts(self, texts):
        return [[1.0] * 384 for _ in texts]


class DocsSyncStateTests(unittest.TestCase):
    def test_ingest_docs_from_config_records_attempt_and_success_state(self):
        captured: dict[str, object] = {}

        class FakeDocsContextStore:
            def __enter__(self):
                return docs.InMemoryDocsStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        class FakeSyncContextStore:
            def __init__(self):
                self.attempts = []
                self.states = []
                self.failures = []

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def record_attempts(self, attempts):
                self.attempts.extend(attempts)
                return len(attempts)

            def upsert_sync_states(self, states):
                self.states.extend(states)
                return len(states)

            def record_failures(self, failures):
                self.failures.extend(failures)
                return len(failures)

        sync_store = FakeSyncContextStore()

        def fake_from_config(config):
            return FakeEmbeddingClient()

        def fake_docs_store_from_database_url(database_url):
            captured["database_url"] = database_url
            return FakeDocsContextStore()

        def fake_sync_store_from_database_url(database_url):
            captured["sync_database_url"] = database_url
            return sync_store

        def fake_ingest_docs(*args, **kwargs):
            return docs.DocsIngestResult(
                source_paths=("AGENTS.md",),
                processed_count=1,
                repo_commit_hash="docs-commit",
            )

        config = AgenticConfig(
            database_url="postgresql://localhost/task405-docs",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

        original_from_config = docs.OllamaEmbeddingClient.from_config
        original_docs_store_factory = docs.PostgresDocsStore.from_database_url
        original_sync_store_factory = docs.PostgresSyncStateStore.from_database_url
        original_ingest = docs.ingest_docs
        try:
            docs.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            docs.PostgresDocsStore.from_database_url = staticmethod(fake_docs_store_from_database_url)
            docs.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            docs.ingest_docs = fake_ingest_docs
            result = docs.ingest_docs_from_config("/workspace", config=config)
        finally:
            docs.OllamaEmbeddingClient.from_config = original_from_config
            docs.PostgresDocsStore.from_database_url = original_docs_store_factory
            docs.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            docs.ingest_docs = original_ingest

        self.assertEqual(result.repo_commit_hash, "docs-commit")
        self.assertEqual(captured["database_url"], "postgresql://localhost/task405-docs")
        self.assertEqual(captured["sync_database_url"], "postgresql://localhost/task405-docs")
        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.states), 1)
        self.assertEqual(sync_store.failures, [])
        self.assertEqual(sync_store.states[0].repo_commit_hash, "docs-commit")
        self.assertIsNone(sync_store.states[0].watermark_timestamp)

    def test_ingest_docs_from_config_records_failure_before_reraising(self):
        class FakeDocsContextStore:
            def __enter__(self):
                return docs.InMemoryDocsStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        class FakeSyncContextStore:
            def __init__(self):
                self.attempts = []
                self.failures = []

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def record_attempts(self, attempts):
                self.attempts.extend(attempts)
                return len(attempts)

            def upsert_sync_states(self, states):
                raise AssertionError("should not persist success state on failure")

            def record_failures(self, failures):
                self.failures.extend(failures)
                return len(failures)

        sync_store = FakeSyncContextStore()

        def fake_from_config(config):
            return FakeEmbeddingClient()

        def fake_docs_store_from_database_url(database_url):
            return FakeDocsContextStore()

        def fake_sync_store_from_database_url(database_url):
            return sync_store

        def fake_ingest_docs(*args, **kwargs):
            raise RuntimeError("boom from docs ingest")

        config = AgenticConfig(
            database_url="postgresql://localhost/task405-docs",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

        original_from_config = docs.OllamaEmbeddingClient.from_config
        original_docs_store_factory = docs.PostgresDocsStore.from_database_url
        original_sync_store_factory = docs.PostgresSyncStateStore.from_database_url
        original_ingest = docs.ingest_docs
        try:
            docs.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            docs.PostgresDocsStore.from_database_url = staticmethod(fake_docs_store_from_database_url)
            docs.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            docs.ingest_docs = fake_ingest_docs
            with self.assertRaisesRegex(RuntimeError, "boom from docs ingest"):
                docs.ingest_docs_from_config("/workspace", config=config)
        finally:
            docs.OllamaEmbeddingClient.from_config = original_from_config
            docs.PostgresDocsStore.from_database_url = original_docs_store_factory
            docs.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            docs.ingest_docs = original_ingest

        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.failures), 1)
        self.assertIn("boom from docs ingest", sync_store.failures[0].error)


if __name__ == "__main__":
    unittest.main()
