from __future__ import annotations

import os
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path
from unittest.mock import patch

from agentic_kb.commands import sync
from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import project
from agentic_kb.sync.state import (
    DEFAULT_PROJECT_NUMBER,
    DEFAULT_PROJECT_OWNER,
)


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


class FakeEmbeddingClient:
    def embed_texts(self, texts):
        return [[1.0] * 384 for _ in texts]


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class ProjectFullRefreshTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)

    def test_sync_project_full_refresh_replays_already_seen_item(self):
        item_node_id = "PVTI_5"
        project_owner = DEFAULT_PROJECT_OWNER
        project_number = DEFAULT_PROJECT_NUMBER

        def fake_ingest_project_items(**kwargs):
            bounds = kwargs["bounds"]
            project_store = kwargs["project_store"]
            if bounds.after_cursor is None:
                item = project.PreparedProjectItem(
                    id=f"github-project-item:{project_owner}/{project_number}:{item_node_id}",
                    project_owner=project_owner,
                    project_number=project_number,
                    project_item_node_id=item_node_id,
                    content_type="Issue",
                    content_id="123",
                    content_node_id="NODE_123",
                    title="ITEM_5",
                    body_text="first body",
                    repo=None,
                    status="In progress",
                    priority="P1",
                    size="M",
                    work_type="feature",
                    area="backend",
                    phase="development",
                    kb_impact="medium",
                    start_date=None,
                    target_date=None,
                    field_values={},
                    html_url="https://github.com/orgs/DripDropz/projects/5#issue-123",
                    source_updated_at=datetime(2026, 3, 29, 13, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
                project_store.upsert_project_items([item])
                return project.ProjectIngestResult(
                    project_owner=project_owner,
                    project_number=project_number,
                    project_title="Daedalus Maintenance",
                    project_url="https://github.com/orgs/DripDropz/projects/5",
                    bounds=bounds,
                    pages_fetched=1,
                    hit_bound=False,
                    final_cursor="cursor-after-item-5",
                    latest_source_updated_at=datetime(2026, 3, 29, 13, 0, tzinfo=timezone.utc),
                    rows_written=1,
                )
            else:
                return project.ProjectIngestResult(
                    project_owner=project_owner,
                    project_number=project_number,
                    project_title="Daedalus Maintenance",
                    project_url="https://github.com/orgs/DripDropz/projects/5",
                    bounds=bounds,
                    pages_fetched=0,
                    hit_bound=False,
                    final_cursor=bounds.after_cursor,
                    latest_source_updated_at=None,
                    rows_written=0,
                )

        config = AgenticConfig(
            database_url=self.database_url,
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minlm",
            github_token="token",
        )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items):
                result_initial = sync.sync_project(
                    workspace_root=Path("/tmp"),
                    config=config,
                )

        self.assertEqual(result_initial["mode"], "initial")
        self.assertEqual(result_initial["final_cursor"], "cursor-after-item-5")

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT body_text FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                (item_node_id,),
            )
            row = cursor.fetchone()
            self.assertIsNotNone(row, "ITEM_5 should exist after initial sync")
            self.assertEqual(row[0], "first body")

        stored_cursor = result_initial["final_cursor"]

        def fake_ingest_project_items_v2(**kwargs):
            bounds = kwargs["bounds"]
            project_store = kwargs["project_store"]
            if bounds.after_cursor is None:
                item = project.PreparedProjectItem(
                    id=f"github-project-item:{project_owner}/{project_number}:{item_node_id}",
                    project_owner=project_owner,
                    project_number=project_number,
                    project_item_node_id=item_node_id,
                    content_type="Issue",
                    content_id="123",
                    content_node_id="NODE_123",
                    title="ITEM_5",
                    body_text="updated after cursor stored",
                    repo=None,
                    status="In progress",
                    priority="P1",
                    size="M",
                    work_type="feature",
                    area="backend",
                    phase="development",
                    kb_impact="medium",
                    start_date=None,
                    target_date=None,
                    field_values={},
                    html_url="https://github.com/orgs/DripDropz/projects/5#issue-123",
                    source_updated_at=datetime(2026, 3, 29, 14, 0, tzinfo=timezone.utc),
                    embedding=[1.0] * 384,
                    metadata={},
                )
                project_store.upsert_project_items([item])
                return project.ProjectIngestResult(
                    project_owner=project_owner,
                    project_number=project_number,
                    project_title="Daedalus Maintenance",
                    project_url="https://github.com/orgs/DripDropz/projects/5",
                    bounds=bounds,
                    pages_fetched=1,
                    hit_bound=False,
                    final_cursor="cursor-after-item-5-v2",
                    latest_source_updated_at=datetime(2026, 3, 29, 14, 0, tzinfo=timezone.utc),
                    rows_written=1,
                )
            else:
                return project.ProjectIngestResult(
                    project_owner=project_owner,
                    project_number=project_number,
                    project_title="Daedalus Maintenance",
                    project_url="https://github.com/orgs/DripDropz/projects/5",
                    bounds=bounds,
                    pages_fetched=0,
                    hit_bound=False,
                    final_cursor=bounds.after_cursor,
                    latest_source_updated_at=None,
                    rows_written=0,
                )

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items_v2):
                result_incremental = sync.sync_project(
                    workspace_root=Path("/tmp"),
                    config=config,
                )

        self.assertEqual(result_incremental["mode"], "incremental")
        self.assertEqual(result_incremental["final_cursor"], stored_cursor)

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT body_text FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                (item_node_id,),
            )
            row = cursor.fetchone()
            self.assertIsNotNone(row, "ITEM_5 should still exist after incremental sync")
            self.assertEqual(row[0], "first body", "ITEM_5 body_text should NOT change on incremental sync")

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items_v2):
                result_full = sync.sync_project(
                    workspace_root=Path("/tmp"),
                    config=config,
                    full_refresh=True,
                )

        self.assertEqual(result_full["mode"], "full")
        self.assertEqual(result_full["final_cursor"], "cursor-after-item-5-v2")

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT body_text FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                (item_node_id,),
            )
            row = cursor.fetchone()
            self.assertIsNotNone(row, "ITEM_5 should exist after full refresh")
            self.assertEqual(row[0], "updated after cursor stored", "ITEM_5 body_text should be updated after full refresh")

        with patch("agentic_kb.commands.sync.OllamaEmbeddingClient.from_config", return_value=FakeEmbeddingClient()):
            with patch("agentic_kb.commands.sync.ingest_project_items", side_effect=fake_ingest_project_items_v2):
                result_return_to_incremental = sync.sync_project(
                    workspace_root=Path("/tmp"),
                    config=config,
                )

        self.assertEqual(result_return_to_incremental["mode"], "incremental")

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT body_text FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                (item_node_id,),
            )
            row = cursor.fetchone()
            self.assertIsNotNone(row, "ITEM_5 should still exist after returning to incremental sync")
            self.assertEqual(row[0], "updated after cursor stored", "ITEM_5 body_text should stay at v2 after returning to incremental")

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT COUNT(*) FROM agentic.kb_project_items WHERE project_item_node_id = %s",
                (item_node_id,),
            )
            count = cursor.fetchone()[0]
            self.assertEqual(count, 1, "Should only have one row for ITEM_5 (upsert in-place)")


def _bootstrap_database(connection) -> None:
    init_sql = (SCHEMA_DIR / "init.sql").read_text(encoding="utf-8")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


if __name__ == "__main__":
    unittest.main()
