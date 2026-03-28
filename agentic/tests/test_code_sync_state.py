from __future__ import annotations

import unittest

from agentic_kb.config import AgenticConfig

try:
    from agentic_kb.ingest import code
except ImportError:  # pragma: no cover - local env may omit tree-sitter extras
    code = None


class FakeEmbeddingClient:
    def embed_texts(self, texts):
        return [[1.0] * 384 for _ in texts]


@unittest.skipIf(code is None, "requires optional tree-sitter dependencies")
class CodeSyncStateTests(unittest.TestCase):
    def test_ingest_code_from_config_records_attempt_and_success_state(self):
        captured: dict[str, object] = {}

        class FakeCodeContextStore:
            def __enter__(self):
                return code.InMemoryCodeChunksStore()

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

        def fake_code_store_from_database_url(database_url):
            captured["database_url"] = database_url
            return FakeCodeContextStore()

        def fake_sync_store_from_database_url(database_url):
            captured["sync_database_url"] = database_url
            return sync_store

        def fake_ingest_code(*args, **kwargs):
            return code.CodeIngestResult(
                source_paths=("source/main/index.ts",),
                processed_file_count=1,
                chunk_count=2,
                repo_commit_hash="code-commit",
            )

        config = AgenticConfig(
            database_url="postgresql://localhost/task405-code",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

        original_from_config = code.OllamaEmbeddingClient.from_config
        original_code_store_factory = code.PostgresCodeChunksStore.from_database_url
        original_sync_store_factory = code.PostgresSyncStateStore.from_database_url
        original_ingest = code.ingest_code
        try:
            code.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            code.PostgresCodeChunksStore.from_database_url = staticmethod(fake_code_store_from_database_url)
            code.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            code.ingest_code = fake_ingest_code
            result = code.ingest_code_from_config("/workspace", config=config)
        finally:
            code.OllamaEmbeddingClient.from_config = original_from_config
            code.PostgresCodeChunksStore.from_database_url = original_code_store_factory
            code.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            code.ingest_code = original_ingest

        self.assertEqual(result.repo_commit_hash, "code-commit")
        self.assertEqual(captured["database_url"], "postgresql://localhost/task405-code")
        self.assertEqual(captured["sync_database_url"], "postgresql://localhost/task405-code")
        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.states), 1)
        self.assertEqual(sync_store.failures, [])
        self.assertEqual(sync_store.states[0].repo_commit_hash, "code-commit")
        self.assertIsNone(sync_store.states[0].watermark_timestamp)

    def test_ingest_code_from_config_records_failure_before_reraising(self):
        class FakeCodeContextStore:
            def __enter__(self):
                return code.InMemoryCodeChunksStore()

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

        def fake_code_store_from_database_url(database_url):
            return FakeCodeContextStore()

        def fake_sync_store_from_database_url(database_url):
            return sync_store

        def fake_ingest_code(*args, **kwargs):
            raise RuntimeError("boom from code ingest")

        config = AgenticConfig(
            database_url="postgresql://localhost/task405-code",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

        original_from_config = code.OllamaEmbeddingClient.from_config
        original_code_store_factory = code.PostgresCodeChunksStore.from_database_url
        original_sync_store_factory = code.PostgresSyncStateStore.from_database_url
        original_ingest = code.ingest_code
        try:
            code.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            code.PostgresCodeChunksStore.from_database_url = staticmethod(fake_code_store_from_database_url)
            code.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            code.ingest_code = fake_ingest_code
            with self.assertRaisesRegex(RuntimeError, "boom from code ingest"):
                code.ingest_code_from_config("/workspace", config=config)
        finally:
            code.OllamaEmbeddingClient.from_config = original_from_config
            code.PostgresCodeChunksStore.from_database_url = original_code_store_factory
            code.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            code.ingest_code = original_ingest

        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.failures), 1)
        self.assertIn("boom from code ingest", sync_store.failures[0].error)


if __name__ == "__main__":
    unittest.main()
