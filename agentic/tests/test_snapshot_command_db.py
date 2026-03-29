from __future__ import annotations

import json
import os
import shutil
import subprocess
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
class SnapshotCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.original_embed_model = os.environ.get("OLLAMA_EMBED_MODEL")
        os.environ["OLLAMA_EMBED_MODEL"] = "all-minilm:l6-v2"
        self.addCleanup(self._restore_embed_model)
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        _seed_snapshot_fixture(self.connection)
        self.connection.commit()

    def test_export_and_destructive_import_round_trip_agentic_schema(self):
        before = status.inspect_database(self.database_url)
        self.assertEqual(before.row_counts["agentic.kb_documents"], 1)

        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-602-roundtrip.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = Path(temp_dir) / "task-602-roundtrip.manifest.json"

            self.assertEqual(export_result.path, dump_path)
            self.assertEqual(export_result.manifest_path, manifest_path)
            self.assertTrue(dump_path.exists())
            self.assertTrue(manifest_path.exists())

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertEqual(manifest["artifact"]["filename"], dump_path.name)
            self.assertEqual(manifest["artifact"]["size_bytes"], dump_path.stat().st_size)
            self.assertEqual(manifest["artifact"]["content_hash"], snapshot.compute_file_sha256(dump_path))
            self.assertEqual(manifest["entity_counts"]["documents"], 1)
            self.assertEqual(manifest["repo"]["docs_commit_hash"], "cafebabe")
            self.assertIsNone(manifest["repo"]["code_commit_hash"])

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT repo_commit_hash, embedding_model, content_hash, manifest, source_path, imported_at "
                    "FROM agentic.kb_snapshot_manifest WHERE snapshot_name = %s",
                    (manifest["snapshot_name"],),
                )
                export_row = cursor.fetchone()

            self.assertIsNotNone(export_row)
            self.assertIsNone(export_row[0])
            self.assertEqual(export_row[1], "all-minilm:l6-v2")
            self.assertEqual(export_row[2], manifest["artifact"]["content_hash"])
            self.assertEqual(export_row[3]["snapshot_name"], manifest["snapshot_name"])
            self.assertEqual(export_row[4], str(manifest_path))
            self.assertIsNone(export_row[5])
            self.connection.commit()

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        """
                        UPDATE agentic.kb_sync_state
                        SET repo_commit_hash = %s
                        WHERE source_name = 'code' AND scope_key = 'repo:DripDropz/daedalus'
                        """,
                        ("cafebabe",),
                    )
            self.connection.commit()

            shared_dump_path = Path(temp_dir) / "task-602-shared-commit.dump"
            shared_result = snapshot.export_snapshot(self.database_url, shared_dump_path)
            shared_manifest_path = shared_result.manifest_path
            self.assertIsNotNone(shared_manifest_path)
            shared_manifest = json.loads(shared_manifest_path.read_text(encoding="utf-8"))

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT repo_commit_hash FROM agentic.kb_snapshot_manifest WHERE snapshot_name = %s",
                    (shared_manifest["snapshot_name"],),
                )
                shared_repo_commit_row = cursor.fetchone()

            self.assertEqual(shared_manifest["repo"]["docs_commit_hash"], "cafebabe")
            self.assertEqual(shared_manifest["repo"]["code_commit_hash"], "cafebabe")
            self.assertEqual(shared_repo_commit_row[0], "cafebabe")
            self.connection.commit()

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents CASCADE"
                    )

            mutated = status.inspect_database(self.database_url)
            self.assertEqual(mutated.row_counts["agentic.kb_documents"], 0)

            import_result = snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)
            self.assertEqual(import_result.path, dump_path)
            self.assertEqual(import_result.manifest_path, manifest_path)

        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.applied_versions, (1, 2, 3))
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)
        docs_summary = next(summary for summary in restored.sync_summaries if summary.source_name == "docs")
        self.assertEqual(docs_summary.row_count, 1)

        with self.connection.cursor() as cursor:
            cursor.execute(
                "SELECT content_hash, source_path, imported_at FROM agentic.kb_snapshot_manifest ORDER BY created_at DESC LIMIT 1"
            )
            import_row = cursor.fetchone()

        self.assertEqual(import_row[0], manifest["artifact"]["content_hash"])
        self.assertEqual(import_row[1], str(manifest_path))
        self.assertIsNotNone(import_row[2])
        self.connection.commit()

    def test_import_fails_before_restore_when_dump_hash_does_not_match_manifest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-602-tampered.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            before_counts = status.inspect_database(self.database_url).row_counts.copy()
            dump_path.write_bytes(b"x" * dump_path.stat().st_size)

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "content hash mismatch"):
                snapshot.import_snapshot(self.database_url, dump_path, confirmed=True)

            after_counts = status.inspect_database(self.database_url).row_counts
            self.assertEqual(after_counts["agentic.kb_documents"], before_counts["agentic.kb_documents"])

    def test_import_fails_before_restore_when_manifest_size_is_tampered(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-602-size-tampered.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            manifest["artifact"]["size_bytes"] += 1
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            before_counts = status.inspect_database(self.database_url).row_counts.copy()

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "size mismatch"):
                snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)

            after_counts = status.inspect_database(self.database_url).row_counts
            self.assertEqual(after_counts["agentic.kb_documents"], before_counts["agentic.kb_documents"])

    def test_import_restricts_restore_to_agentic_schema_for_nonconforming_dump(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-205-full-db.dump"

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("DROP TABLE IF EXISTS public.task205_external_restore")
                    cursor.execute("CREATE TABLE public.task205_external_restore (id integer PRIMARY KEY)")
                    cursor.execute("INSERT INTO public.task205_external_restore (id) VALUES (7)")

            subprocess.run(
                [
                    "pg_dump",
                    "--format=custom",
                    "--compress=6",
                    "--no-owner",
                    "--no-privileges",
                    f"--file={dump_path}",
                    self.database_url,
                ],
                check=True,
                capture_output=True,
                text=True,
            )

            manifest_path = Path(temp_dir) / "task-205-full-db.manifest.json"
            manifest = {
                "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
                "schema_version": 1,
                "snapshot_name": "task-205-full-db",
                "snapshot_created_at": "2026-03-29T02:00:00Z",
                "artifact": {
                    "filename": dump_path.name,
                    "dump_format": "postgresql_custom",
                    "compression": {"algorithm": "gzip", "level": 6},
                    "size_bytes": dump_path.stat().st_size,
                    "content_hash": snapshot.compute_file_sha256(dump_path),
                },
                "repo": {
                    "name": "DripDropz/daedalus",
                    "docs_commit_hash": None,
                    "code_commit_hash": None,
                },
                "embedding_model": "all-minilm:l6-v2",
                "entity_counts": {
                    "documents": 1,
                    "code_chunks": 0,
                    "github_issues": 0,
                    "github_issue_comments": 0,
                    "github_prs": 0,
                    "github_pr_comments": 0,
                    "project_items": 0,
                },
                "sync_state": {
                    "docs": {"repo_commit_hash": None, "last_synced_at": None},
                    "code": {"repo_commit_hash": None, "last_synced_at": None},
                    "github": {
                        "issues": {"updated_at_watermark": None},
                        "pulls": {"updated_at_watermark": None},
                        "issue_comments": {"updated_at_watermark": None},
                        "review_comments": {"updated_at_watermark": None},
                    },
                    "project": {
                        "owner": "DripDropz",
                        "number": 5,
                        "cursor": None,
                        "updated_at_watermark": None,
                    },
                },
            }
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("DROP TABLE public.task205_external_restore")
                    cursor.execute("TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_documents")

            snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)

            restored = status.inspect_database(self.database_url)
            self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)

            with self.connection.cursor() as cursor:
                cursor.execute(
                    "SELECT to_regclass('public.task205_external_restore') IS NOT NULL"
                )
                public_table_present = bool(cursor.fetchone()[0])

            self.assertFalse(public_table_present)

    def test_import_succeeds_for_fresh_database_without_agentic_schema(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-612-fresh-schema-missing.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")

            import_result = snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)

        self.assertEqual(import_result.path, dump_path)
        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)

    def test_import_succeeds_for_initialized_but_empty_kb(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-612-empty-kb.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute(
                        "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents CASCADE"
                    )

            import_result = snapshot.import_snapshot(self.database_url, dump_path, confirmed=True)

        self.assertEqual(import_result.manifest_path, manifest_path)
        restored = status.inspect_database(self.database_url)
        self.assertEqual(restored.row_counts["agentic.kb_documents"], 1)

    def test_import_rejects_seeded_documents_before_restore(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-612-seeded-docs.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            before_counts = status.inspect_database(self.database_url).row_counts.copy()

            with self.assertRaisesRegex(
                snapshot.SnapshotCommandError,
                "fresh, isolated, or otherwise disposable KB database",
            ):
                snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)

            after_counts = status.inspect_database(self.database_url).row_counts
            self.assertEqual(after_counts["agentic.kb_documents"], before_counts["agentic.kb_documents"])

    def test_import_rejects_leftover_sync_state_before_restore(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-612-seeded-sync-state.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents CASCADE")
                    cursor.execute(
                        """
                        INSERT INTO agentic.kb_sync_state (
                            id,
                            source_name,
                            scope_key,
                            repo_commit_hash,
                            metadata
                        ) VALUES (
                            'sync-state:docs:repo:DripDropz/daedalus',
                            'docs',
                            'repo:DripDropz/daedalus',
                            'deadbeef',
                            '{"repo": "DripDropz/daedalus"}'::jsonb
                        )
                        """
                    )

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "agentic.kb_sync_state=1"):
                snapshot.import_snapshot(self.database_url, dump_path, confirmed=True)

            restored_counts = status.inspect_database(self.database_url).row_counts
            self.assertEqual(restored_counts["agentic.kb_documents"], 0)

    def test_import_rejects_leftover_snapshot_manifest_before_restore(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "task-612-seeded-manifest.dump"
            export_result = snapshot.export_snapshot(self.database_url, dump_path)
            manifest_path = export_result.manifest_path
            self.assertIsNotNone(manifest_path)

            with self.connection.transaction():
                with self.connection.cursor() as cursor:
                    cursor.execute("TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents CASCADE")
                    cursor.execute(
                        """
                        INSERT INTO agentic.kb_snapshot_manifest (
                            id,
                            snapshot_name,
                            schema_version,
                            snapshot_created_at,
                            manifest,
                            entity_counts,
                            github_watermarks,
                            content_hash
                        ) VALUES (
                            'snapshot-manifest:task-612-leftover',
                            'leftover',
                            1,
                            %s,
                            '{}'::jsonb,
                            '{}'::jsonb,
                            '{}'::jsonb,
                            'sha256:' || repeat('0', 64)
                        )
                        """,
                        (datetime(2026, 3, 29, 21, 0, tzinfo=timezone.utc),),
                    )

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "agentic.kb_snapshot_manifest=1"):
                snapshot.import_snapshot(self.database_url, manifest_path, confirmed=True)

            restored_counts = status.inspect_database(self.database_url).row_counts
            self.assertEqual(restored_counts["agentic.kb_documents"], 0)

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


def _seed_snapshot_fixture(connection) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute(
                "TRUNCATE TABLE agentic.kb_snapshot_manifest, agentic.kb_sync_state, agentic.kb_documents CASCADE"
            )
            cursor.execute(
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
                    content_hash
                ) VALUES (
                    'docs:snapshot-db#0',
                    'docs',
                    'plan',
                    '.agent/plans/agentic/task-plans/task-205.md',
                    'snapshot plan',
                    '[]'::jsonb,
                    0,
                    'snapshot content',
                    'snapshot content',
                    'snapshot-doc-hash'
                )
                """
            )
            cursor.execute(
                """
                INSERT INTO agentic.kb_sync_state (
                    id,
                    source_name,
                    scope_key,
                    repo_commit_hash,
                    last_attempted_at,
                    last_succeeded_at,
                    metadata
                ) VALUES (
                    'sync-state:docs:repo:DripDropz/daedalus',
                    'docs',
                    'repo:DripDropz/daedalus',
                    'cafebabe',
                    %s,
                    %s,
                    '{"repo": "DripDropz/daedalus"}'::jsonb
                )
                """,
                (
                    datetime(2026, 3, 28, 23, 20, tzinfo=timezone.utc),
                    datetime(2026, 3, 28, 23, 21, tzinfo=timezone.utc),
                ),
            )
            cursor.execute(
                """
                INSERT INTO agentic.kb_sync_state (
                    id,
                    source_name,
                    scope_key,
                    repo_commit_hash,
                    last_attempted_at,
                    last_succeeded_at,
                    metadata
                ) VALUES (
                    'sync-state:code:repo:DripDropz/daedalus',
                    'code',
                    'repo:DripDropz/daedalus',
                    NULL,
                    %s,
                    %s,
                    '{"repo": "DripDropz/daedalus"}'::jsonb
                )
                """,
                (
                    datetime(2026, 3, 28, 23, 22, tzinfo=timezone.utc),
                    datetime(2026, 3, 28, 23, 23, tzinfo=timezone.utc),
                ),
            )


def _sanitized_sql(path: Path) -> str:
    lines = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


if __name__ == "__main__":
    unittest.main()
