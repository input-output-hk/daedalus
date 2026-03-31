from __future__ import annotations

import json
import os
import shutil
import tempfile
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import snapshot, status


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV)
    and find_spec("psycopg") is not None
    and shutil.which("pg_dump") is not None
    and shutil.which("pg_restore") is not None
    and shutil.which("psql") is not None,
    "requires AGENTIC_TEST_DATABASE_URL, psycopg, and PostgreSQL client tools",
)
class SnapshotRoundTripTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.original_embed_model = os.environ.get("OLLAMA_EMBED_MODEL")
        os.environ["OLLAMA_EMBED_MODEL"] = "all-minilm:l6-v2"
        self.addCleanup(self._restore_embed_model)
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        self.connection.commit()

    def test_full_round_trip_all_entities(self):
        _seed_all_entities(self.connection)
        self.connection.commit()

        before = status.inspect_database(self.database_url)
        self.assertEqual(before.row_counts["agentic.kb_documents"], 3)
        self.assertEqual(before.row_counts["agentic.kb_code_chunks"], 2)
        self.assertEqual(before.row_counts["agentic.kb_github_issues"], 2)
        self.assertEqual(before.row_counts["agentic.kb_github_issue_comments"], 2)
        self.assertEqual(before.row_counts["agentic.kb_github_prs"], 2)
        self.assertEqual(before.row_counts["agentic.kb_github_pr_comments"], 2)
        self.assertEqual(before.row_counts["agentic.kb_project_items"], 2)

        pre_queries = _capture_deterministic_queries(self.connection)
        pre_sync_state = _capture_sync_state(self.connection)

        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "roundtrip-all.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path

            self.assertTrue(dump_path.exists())
            self.assertTrue(manifest_path.exists())

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertEqual(manifest["entity_counts"]["documents"], 3)
            self.assertEqual(manifest["entity_counts"]["code_chunks"], 2)
            self.assertEqual(manifest["entity_counts"]["github_issues"], 2)
            self.assertEqual(manifest["entity_counts"]["github_issue_comments"], 2)
            self.assertEqual(manifest["entity_counts"]["github_prs"], 2)
            self.assertEqual(manifest["entity_counts"]["github_pr_comments"], 2)
            self.assertEqual(manifest["entity_counts"]["project_items"], 2)

            self.assertEqual(manifest["artifact"]["filename"], dump_path.name)
            self.assertEqual(manifest["artifact"]["size_bytes"], dump_path.stat().st_size)
            self.assertEqual(manifest["artifact"]["content_hash"], snapshot.compute_file_sha256(dump_path))
            self.assertEqual(manifest["embedding_contract"]["embedding_model"], "all-minilm:l6-v2")
            self.assertEqual(manifest["embedding_contract"]["embedding_dimension"], 384)

            self.assertIn("docs", manifest["sync_state"])
            self.assertIn("code", manifest["sync_state"])
            self.assertIn("github", manifest["sync_state"])
            self.assertIn("project", manifest["sync_state"])

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, "
                        "agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, "
                        "agentic.kb_github_issue_comments, agentic.kb_github_prs, "
                        "agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE"
                    )

            mutated = status.inspect_database(self.database_url)
            self.assertEqual(mutated.row_counts["agentic.kb_documents"], 0)
            self.assertEqual(mutated.row_counts["agentic.kb_code_chunks"], 0)

            import_result = snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)
            self.assertEqual(import_result.path, dump_path)
            self.assertEqual(import_result.manifest_path, manifest_path)

        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.applied_versions, (1, 2, 3))
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 3)
        self.assertEqual(restored.row_counts["agentic.kb_code_chunks"], 2)
        self.assertEqual(restored.row_counts["agentic.kb_github_issues"], 2)
        self.assertEqual(restored.row_counts["agentic.kb_github_issue_comments"], 2)
        self.assertEqual(restored.row_counts["agentic.kb_github_prs"], 2)
        self.assertEqual(restored.row_counts["agentic.kb_github_pr_comments"], 2)
        self.assertEqual(restored.row_counts["agentic.kb_project_items"], 2)

        docs_summary = next(s for s in restored.sync_summaries if s.source_name == "docs")
        self.assertEqual(docs_summary.row_count, 1)
        code_summary = next(s for s in restored.sync_summaries if s.source_name == "code")
        self.assertEqual(code_summary.row_count, 1)
        github_summary = next(s for s in restored.sync_summaries if s.source_name == "github")
        self.assertEqual(github_summary.row_count, 4)
        project_summary = next(s for s in restored.sync_summaries if s.source_name == "project")
        self.assertEqual(project_summary.row_count, 1)

        post_sync_state = _capture_sync_state(self.connection)
        self.assertEqual(pre_sync_state, post_sync_state)

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT content_hash, source_path, imported_at FROM agentic.kb_snapshot_manifest ORDER BY created_at DESC LIMIT 1"
            )
            import_row = cursor.fetchone()

        self.assertEqual(import_row[0], manifest["artifact"]["content_hash"])
        self.assertEqual(import_row[1], str(manifest_path))
        self.assertIsNotNone(import_row[2])
        self.connection.commit()

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT entity_counts, github_watermarks FROM agentic.kb_snapshot_manifest ORDER BY created_at DESC LIMIT 1"
            )
            manifest_row = cursor.fetchone()

        self.assertEqual(manifest_row[0]["documents"], 3)
        self.assertEqual(manifest_row[0]["code_chunks"], 2)
        self.assertEqual(manifest_row[0]["github_issues"], 2)
        self.assertEqual(manifest_row[0]["project_items"], 2)
        self.assertIn("issues", manifest_row[1])
        self.assertIn("pulls", manifest_row[1])
        self.connection.commit()

        post_queries = _capture_deterministic_queries(self.connection)
        self.assertEqual(pre_queries, post_queries)

    def test_multi_manifest_round_trip(self):
        _seed_all_entities(self.connection)
        self.connection.commit()

        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path_1 = Path(temp_dir) / "roundtrip-multi-1.dump"
            export_result_1 = snapshot.export_snapshot(self.database_url, dump_path_1)
            manifest_path_1 = export_result_1.manifest_path
            manifest_1 = json.loads(manifest_path_1.read_text(encoding="utf-8"))

            with self.connection.cursor() as cursor:
                cursor.execute("SELECT COUNT(*) FROM agentic.kb_snapshot_manifest")
                count_after_first = cursor.fetchone()[0]
            self.assertGreaterEqual(count_after_first, 1)
            self.connection.commit()

            _seed_additional_entities(self.connection)
            self.connection.commit()

            dump_path_2 = Path(temp_dir) / "roundtrip-multi-2.dump"
            export_result_2 = snapshot.export_snapshot(self.database_url, dump_path_2)
            manifest_path_2 = export_result_2.manifest_path
            manifest_2 = json.loads(manifest_path_2.read_text(encoding="utf-8"))

            with self.connection.cursor() as cursor:
                cursor.execute("SELECT COUNT(*) FROM agentic.kb_snapshot_manifest")
                count_after_second = cursor.fetchone()[0]
            self.assertGreaterEqual(count_after_second, 2)
            self.connection.commit()

            self.assertGreater(manifest_2["entity_counts"]["documents"], manifest_1["entity_counts"]["documents"])
            self.assertGreater(manifest_2["entity_counts"]["code_chunks"], manifest_1["entity_counts"]["code_chunks"])
            self.assertGreater(manifest_2["entity_counts"]["github_issues"], manifest_1["entity_counts"]["github_issues"])

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, "
                        "agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, "
                        "agentic.kb_github_issue_comments, agentic.kb_github_prs, "
                        "agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE"
                    )

            import_result = snapshot.import_snapshot(self.database_url, manifest_path_2, confirmed=True)
            self.assertEqual(import_result.path, dump_path_2)

        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.row_counts["agentic.kb_documents"], manifest_2["entity_counts"]["documents"])
        self.assertEqual(restored.row_counts["agentic.kb_code_chunks"], manifest_2["entity_counts"]["code_chunks"])
        self.assertEqual(restored.row_counts["agentic.kb_github_issues"], manifest_2["entity_counts"]["github_issues"])

        with self.connection.cursor() as cursor:
            cursor.execute("SELECT COUNT(*) FROM agentic.kb_snapshot_manifest")
            manifest_count = cursor.fetchone()[0]
        self.assertEqual(manifest_count, 1)
        self.connection.commit()

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT content_hash, entity_counts FROM agentic.kb_snapshot_manifest LIMIT 1"
            )
            persisted = cursor.fetchone()
        self.assertEqual(persisted[0], manifest_2["artifact"]["content_hash"])
        self.assertEqual(persisted[1]["documents"], manifest_2["entity_counts"]["documents"])
        self.connection.commit()

    def test_round_trip_with_sparse_entities(self):
        _seed_sparse_entities(self.connection)
        self.connection.commit()

        before = status.inspect_database(self.database_url)
        self.assertEqual(before.row_counts["agentic.kb_documents"], 3)
        self.assertEqual(before.row_counts["agentic.kb_code_chunks"], 0)
        self.assertEqual(before.row_counts["agentic.kb_github_issues"], 0)
        self.assertEqual(before.row_counts["agentic.kb_github_issue_comments"], 0)
        self.assertEqual(before.row_counts["agentic.kb_github_prs"], 0)
        self.assertEqual(before.row_counts["agentic.kb_github_pr_comments"], 0)
        self.assertEqual(before.row_counts["agentic.kb_project_items"], 2)

        pre_queries = _capture_deterministic_queries(self.connection)
        pre_sync_state = _capture_sync_state(self.connection)

        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "roundtrip-sparse.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertEqual(manifest["entity_counts"]["documents"], 3)
            self.assertEqual(manifest["entity_counts"]["code_chunks"], 0)
            self.assertEqual(manifest["entity_counts"]["github_issues"], 0)
            self.assertEqual(manifest["entity_counts"]["github_issue_comments"], 0)
            self.assertEqual(manifest["entity_counts"]["github_prs"], 0)
            self.assertEqual(manifest["entity_counts"]["github_pr_comments"], 0)
            self.assertEqual(manifest["entity_counts"]["project_items"], 2)

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, "
                        "agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, "
                        "agentic.kb_github_issue_comments, agentic.kb_github_prs, "
                        "agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE"
                    )

            import_result = snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)
            self.assertEqual(import_result.path, dump_path)

        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 3)
        self.assertEqual(restored.row_counts["agentic.kb_code_chunks"], 0)
        self.assertEqual(restored.row_counts["agentic.kb_github_issues"], 0)
        self.assertEqual(restored.row_counts["agentic.kb_github_issue_comments"], 0)
        self.assertEqual(restored.row_counts["agentic.kb_github_prs"], 0)
        self.assertEqual(restored.row_counts["agentic.kb_github_pr_comments"], 0)
        self.assertEqual(restored.row_counts["agentic.kb_project_items"], 2)
        self.assertEqual(restored.applied_versions, (1, 2, 3))

        post_sync_state = _capture_sync_state(self.connection)
        self.assertEqual(pre_sync_state, post_sync_state)

        post_queries = _capture_deterministic_queries(self.connection)
        self.assertEqual(pre_queries, post_queries)

    def _restore_embed_model(self):
        if self.original_embed_model is None:
            os.environ.pop("OLLAMA_EMBED_MODEL", None)
        else:
            os.environ["OLLAMA_EMBED_MODEL"] = self.original_embed_model


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
        stripped = line.strip()
        if stripped.startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


def _seed_all_entities(connection) -> None:
    _watermark = datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc)
    _now = datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc)
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute(
                "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, "
                "agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, "
                "agentic.kb_github_issue_comments, agentic.kb_github_prs, "
                "agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE"
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_documents (id, source_domain, doc_kind, source_path, title, heading_path, chunk_index, content, preview_text, content_hash)
                VALUES
                    ('docs:plan#0', 'docs', 'plan', '.agent/plans/task-100.md', 'Plan Alpha', '[]'::jsonb, 0, 'plan content alpha', 'plan alpha', 'hash-plan-0'),
                    ('docs:code#0', 'docs', 'code', 'docs/api.md', 'API Reference', '[]'::jsonb, 0, 'api content', 'api ref', 'hash-api-0'),
                    ('docs:arch#0', 'docs', 'architecture', 'docs/architecture.md', 'Architecture', '[]'::jsonb, 0, 'arch content', 'arch', 'hash-arch-0')
                """
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_code_chunks (id, repo_path, language, symbol_name, symbol_kind, chunk_index, start_line, end_line, content, preview_text, content_hash)
                VALUES
                    ('code:py#0', 'src/app.py', 'python', 'main', 'function', 0, 1, 10, 'def main(): pass', 'main func', 'hash-py-0'),
                    ('code:ts#0', 'src/index.ts', 'typescript', 'init', 'function', 0, 1, 5, 'function init() {}', 'init func', 'hash-ts-0')
                """
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_github_issues (id, repo, issue_number, title, state, body_text, preview_text, html_url, source_created_at, source_updated_at)
                VALUES
                    ('gh:issue:1', 'DripDropz/daedalus', 1, 'Open Issue', 'open', 'issue body 1', 'open issue', 'https://github.com/DripDropz/daedalus/issues/1', %s, %s),
                    ('gh:issue:2', 'DripDropz/daedalus', 2, 'Closed Issue', 'closed', 'issue body 2', 'closed issue', 'https://github.com/DripDropz/daedalus/issues/2', %s, %s)
                """,
                (_now, _watermark, _now, _watermark),
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_github_prs (id, repo, pr_number, title, state, body_text, preview_text, html_url, source_created_at, source_updated_at)
                VALUES
                    ('gh:pr:1', 'DripDropz/daedalus', 10, 'Open PR', 'open', 'pr body 1', 'open pr', 'https://github.com/DripDropz/daedalus/pull/10', %s, %s),
                    ('gh:pr:2', 'DripDropz/daedalus', 11, 'Merged PR', 'closed', 'pr body 2', 'merged pr', 'https://github.com/DripDropz/daedalus/pull/11', %s, %s)
                """,
                (_now, _watermark, _now, _watermark),
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_project_items (id, project_owner, project_number, project_item_node_id, title, body_text, status, source_updated_at)
                VALUES
                    ('proj:item:1', 'DripDropz', 5, 'node-1', 'Project Item A', 'body A', 'In Progress', %s),
                    ('proj:item:2', 'DripDropz', 5, 'node-2', 'Project Item B', 'body B', 'Done', %s)
                """,
                (_watermark, _watermark),
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_github_issue_comments (id, issue_id, repo, issue_number, body_text, preview_text, html_url, source_created_at, source_updated_at)
                VALUES
                    ('gh:ic:1', 'gh:issue:1', 'DripDropz/daedalus', 1, 'comment on issue 1', 'comment 1', 'https://github.com/DripDropz/daedalus/issues/1#issuecomment-1', %s, %s),
                    ('gh:ic:2', 'gh:issue:2', 'DripDropz/daedalus', 2, 'comment on issue 2', 'comment 2', 'https://github.com/DripDropz/daedalus/issues/2#issuecomment-2', %s, %s)
                """,
                (_now, _watermark, _now, _watermark),
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_github_pr_comments (id, pr_id, repo, pr_number, comment_type, body_text, preview_text, html_url, source_created_at, source_updated_at)
                VALUES
                    ('gh:prc:1', 'gh:pr:1', 'DripDropz/daedalus', 10, 'issue_comment', 'comment on pr 10', 'prc 1', 'https://github.com/DripDropz/daedalus/pull/10#issuecomment-1', %s, %s),
                    ('gh:prc:2', 'gh:pr:2', 'DripDropz/daedalus', 11, 'review_comment', 'review on pr 11', 'prc 2', 'https://github.com/DripDropz/daedalus/pull/11#discussion-1', %s, %s)
                """,
                (_now, _watermark, _now, _watermark),
            )

            _seed_sync_state_all_sources(cursor, _watermark)


def _seed_sparse_entities(connection) -> None:
    _watermark = datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc)
    _now = datetime(2026, 3, 28, 12, 0, tzinfo=timezone.utc)
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute(
                "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, "
                "agentic.kb_documents, agentic.kb_code_chunks, agentic.kb_github_issues, "
                "agentic.kb_github_issue_comments, agentic.kb_github_prs, "
                "agentic.kb_github_pr_comments, agentic.kb_project_items CASCADE"
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_documents (id, source_domain, doc_kind, source_path, title, heading_path, chunk_index, content, preview_text, content_hash)
                VALUES
                    ('docs:sparse:0', 'docs', 'plan', '.agent/plans/sparse-0.md', 'Sparse Doc 0', '[]'::jsonb, 0, 'sparse content 0', 'sparse 0', 'hash-sparse-0'),
                    ('docs:sparse:1', 'docs', 'code', 'docs/sparse-1.md', 'Sparse Doc 1', '[]'::jsonb, 0, 'sparse content 1', 'sparse 1', 'hash-sparse-1'),
                    ('docs:sparse:2', 'docs', 'architecture', 'docs/sparse-2.md', 'Sparse Doc 2', '[]'::jsonb, 0, 'sparse content 2', 'sparse 2', 'hash-sparse-2')
                """
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_project_items (id, project_owner, project_number, project_item_node_id, title, body_text, status, source_updated_at)
                VALUES
                    ('proj:sparse:1', 'DripDropz', 5, 'sparse-node-1', 'Sparse Item A', 'sparse body A', 'Todo', %s),
                    ('proj:sparse:2', 'DripDropz', 5, 'sparse-node-2', 'Sparse Item B', 'sparse body B', 'In Progress', %s)
                """,
                (_watermark, _watermark),
            )

            _seed_sync_state_all_sources(cursor, _watermark)


def _seed_additional_entities(connection) -> None:
    _watermark = datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc)
    _now = datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc)
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute(
                """
                INSERT INTO agentic.kb_documents (id, source_domain, doc_kind, source_path, title, heading_path, chunk_index, content, preview_text, content_hash)
                VALUES
                    ('docs:plan#1', 'docs', 'plan', '.agent/plans/task-200.md', 'Plan Beta', '[]'::jsonb, 0, 'plan content beta', 'plan beta', 'hash-plan-1'),
                    ('docs:plan#2', 'docs', 'plan', '.agent/plans/task-300.md', 'Plan Gamma', '[]'::jsonb, 0, 'plan content gamma', 'plan gamma', 'hash-plan-2')
                """
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_code_chunks (id, repo_path, language, symbol_name, symbol_kind, chunk_index, start_line, end_line, content, preview_text, content_hash)
                VALUES
                    ('code:py#1', 'src/utils.py', 'python', 'helper', 'function', 0, 1, 8, 'def helper(): pass', 'helper func', 'hash-py-1')
                """
            )

            cursor.execute(
                """
                INSERT INTO agentic.kb_github_issues (id, repo, issue_number, title, state, body_text, preview_text, html_url, source_created_at, source_updated_at)
                VALUES
                    ('gh:issue:3', 'DripDropz/daedalus', 3, 'Additional Issue', 'open', 'issue body 3', 'additional issue', 'https://github.com/DripDropz/daedalus/issues/3', %s, %s)
                """,
                (_now, _watermark),
            )


def _seed_sync_state_all_sources(cursor, watermark: datetime) -> None:
    _attempted = watermark
    _succeeded = watermark

    cursor.execute(
        """
        INSERT INTO agentic.kb_sync_state (id, source_name, scope_key, repo_commit_hash, watermark_timestamp, last_attempted_at, last_succeeded_at, metadata)
        VALUES
            ('sync-state:docs:repo:DripDropz/daedalus', 'docs', 'repo:DripDropz/daedalus', 'cafebabe', %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:code:repo:DripDropz/daedalus', 'code', 'repo:DripDropz/daedalus', 'deadbeef', %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:github:issues:repo:DripDropz/daedalus', 'github', 'stream:issues:repo:DripDropz/daedalus', NULL, %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:github:pulls:repo:DripDropz/daedalus', 'github', 'stream:pulls:repo:DripDropz/daedalus', NULL, %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:github:issue_comments:repo:DripDropz/daedalus', 'github', 'stream:issue_comments:repo:DripDropz/daedalus', NULL, %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:github:review_comments:repo:DripDropz/daedalus', 'github', 'stream:review_comments:repo:DripDropz/daedalus', NULL, %s, %s, %s, '{"repo": "DripDropz/daedalus"}'::jsonb),
            ('sync-state:project:project:DripDropz:5', 'project', 'project:DripDropz:5', NULL, %s, %s, %s, '{"owner": "DripDropz", "number": 5}'::jsonb)
        """,
        (
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
            watermark, _attempted, _succeeded,
        ),
    )


def _capture_deterministic_queries(connection) -> dict[str, list[tuple]]:
    results: dict[str, list[tuple]] = {}
    with connection.cursor() as cursor:
        cursor.execute("SELECT id, title, content_hash FROM agentic.kb_documents ORDER BY id")
        results["documents"] = cursor.fetchall()

        cursor.execute("SELECT id, symbol_name, content_hash FROM agentic.kb_code_chunks ORDER BY id")
        results["code_chunks"] = cursor.fetchall()

        cursor.execute("SELECT id, title, state FROM agentic.kb_github_issues ORDER BY id")
        results["github_issues"] = cursor.fetchall()

        cursor.execute("SELECT id, body_text FROM agentic.kb_github_issue_comments ORDER BY id")
        results["github_issue_comments"] = cursor.fetchall()

        cursor.execute("SELECT id, title, state FROM agentic.kb_github_prs ORDER BY id")
        results["github_prs"] = cursor.fetchall()

        cursor.execute("SELECT id, body_text FROM agentic.kb_github_pr_comments ORDER BY id")
        results["github_pr_comments"] = cursor.fetchall()

        cursor.execute("SELECT id, title, status FROM agentic.kb_project_items ORDER BY id")
        results["project_items"] = cursor.fetchall()

    return results


def _capture_sync_state(connection) -> list[tuple]:
    with connection.cursor() as cursor:
        cursor.execute(
            "SELECT source_name, scope_key, repo_commit_hash, watermark_timestamp "
            "FROM agentic.kb_sync_state ORDER BY source_name, scope_key"
        )
        return cursor.fetchall()


if __name__ == "__main__":
    unittest.main()
