# Research: task-713 Removal Reconciliation Tests

## What the tests cover

Nine regression tests proving that deleted docs/code files and removed/cleared Project metadata stop surfacing after sync or explicit full-refresh runs against an already-seeded KB.

- **Docs removal**: Combined add+delete scenario proving deleted document paths are removed while new ones are added and existing ones updated in a single sync run.
- **Code removal**: Supported-path filtering proving unsupported file types are pruned from the code index, plus combined add+delete for supported files.
- **Project removal**: Multi-item convergence proving archived items and unseen items are both removed after `sync project --full`, cleared field values (null) are preserved through `ON CONFLICT DO UPDATE`, and incremental sync is a no-op when no archived items exist.
- **DB-backed verification**: Three PostgreSQL-backed tests confirming the same removal behavior persists through the real database layer.

## Key patterns used

- `unittest.TestCase` with `InMemoryDocsStore`, `InMemoryCodeChunksStore`, `InMemoryProjectItemsStore`, and `InMemorySyncStateStore` for in-memory tests.
- `FakeEmbeddingClient` imported from `test_docs_ingest` to avoid real embedding calls.
- `@unittest.skipUnless(AGENTIC_TEST_DATABASE_URL, ...)` for DB-backed tests following `test_sync_command_db.py` conventions.
- Store-level verification via `rows_by_key` inspection and direct SQL queries — not just mock call counts.

## Durable findings

No new research beyond implementation capture. The test patterns established here are consistent with the existing test suite (tasks 704, 708, 711, 712) and do not introduce novel testing approaches.
