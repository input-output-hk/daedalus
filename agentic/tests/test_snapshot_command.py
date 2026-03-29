from __future__ import annotations

import hashlib
import json
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import MagicMock, patch

from agentic_kb.commands import snapshot


class SnapshotCommandTests(unittest.TestCase):
    def test_resolve_export_path_defaults_under_snapshots_mount(self):
        path = snapshot.resolve_export_path(None, now=datetime(2026, 3, 28, 23, 0, tzinfo=timezone.utc))
        self.assertEqual(
            path,
            Path("/workspace/agentic/snapshots/agentic-kb-20260328T230000Z.dump"),
        )

    def test_resolve_export_path_appends_name_for_existing_directory(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            path = snapshot.resolve_export_path(
                temp_dir,
                now=datetime(2026, 3, 28, 23, 5, tzinfo=timezone.utc),
            )
            self.assertEqual(
                path,
                Path(temp_dir) / "agentic-kb-20260328T230500Z.dump",
            )

    def test_resolve_export_path_requires_dump_suffix(self):
        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "must end with .dump"):
            snapshot.resolve_export_path("snapshot.sql")

    def test_derive_manifest_path_from_dump_path(self):
        dump_path = Path("/tmp/agentic-kb-20260329T020000Z.dump")
        self.assertEqual(
            snapshot.derive_manifest_path_from_dump_path(dump_path),
            Path("/tmp/agentic-kb-20260329T020000Z.manifest.json"),
        )

    def test_resolve_import_path_rejects_missing_file(self):
        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "does not exist"):
            snapshot.resolve_import_path("missing.dump")

    def test_resolve_import_snapshot_pair_from_dump_path_requires_sibling_manifest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "snapshot.dump"
            dump_path.write_bytes(b"dump")

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "manifest does not exist"):
                snapshot.resolve_import_snapshot_pair(dump_path)

    def test_resolve_import_snapshot_pair_from_manifest_uses_manifest_artifact_filename(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "snapshot.dump"
            dump_path.write_bytes(b"dump-bytes")
            manifest_path = Path(temp_dir) / "snapshot.manifest.json"
            manifest = _manifest_fixture(
                artifact_filename=dump_path.name,
                artifact_size_bytes=dump_path.stat().st_size,
                artifact_content_hash=snapshot.compute_file_sha256(dump_path),
            )
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            pair, loaded_manifest = snapshot.resolve_import_snapshot_pair(manifest_path)

        self.assertEqual(pair.dump_path, dump_path)
        self.assertEqual(pair.manifest_path, manifest_path)
        self.assertEqual(loaded_manifest["artifact"]["filename"], dump_path.name)

    def test_resolve_import_snapshot_pair_rejects_manifest_with_directory_artifact_filename(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "snapshot.dump"
            dump_path.write_bytes(b"dump-bytes")
            manifest_path = Path(temp_dir) / "snapshot.manifest.json"
            manifest = _manifest_fixture(
                artifact_filename="nested/snapshot.dump",
                artifact_size_bytes=dump_path.stat().st_size,
                artifact_content_hash=snapshot.compute_file_sha256(dump_path),
            )
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "invalid snapshot manifest"):
                snapshot.resolve_import_snapshot_pair(manifest_path)

    def test_resolve_import_snapshot_pair_rejects_manifest_artifact_without_dump_suffix(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            wrong_dump_path = Path(temp_dir) / "snapshot.sql"
            wrong_dump_path.write_bytes(b"dump-bytes")
            manifest_path = Path(temp_dir) / "snapshot.manifest.json"
            manifest = _manifest_fixture(
                artifact_filename=wrong_dump_path.name,
                artifact_size_bytes=wrong_dump_path.stat().st_size,
                artifact_content_hash=snapshot.compute_file_sha256(wrong_dump_path),
            )
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "must end with .dump"):
                snapshot.resolve_import_snapshot_pair(manifest_path)

    def test_compute_file_sha256_streams_expected_digest(self):
        with tempfile.NamedTemporaryFile() as temp_file:
            payload = b"snapshot-content"
            Path(temp_file.name).write_bytes(payload)

            digest = snapshot.compute_file_sha256(Path(temp_file.name))

        self.assertEqual(digest, f"sha256:{hashlib.sha256(payload).hexdigest()}")

    def test_validate_snapshot_artifact_rejects_size_mismatch(self):
        with tempfile.NamedTemporaryFile() as temp_file:
            dump_path = Path(temp_file.name)
            dump_path.write_bytes(b"payload")
            manifest = _manifest_fixture(
                artifact_filename=dump_path.name,
                artifact_size_bytes=999,
                artifact_content_hash=snapshot.compute_file_sha256(dump_path),
            )

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "size mismatch"):
                snapshot.validate_snapshot_artifact(dump_path, manifest)

    def test_validate_snapshot_artifact_rejects_content_hash_mismatch(self):
        with tempfile.NamedTemporaryFile() as temp_file:
            dump_path = Path(temp_file.name)
            dump_path.write_bytes(b"payload")
            manifest = _manifest_fixture(
                artifact_filename=dump_path.name,
                artifact_size_bytes=dump_path.stat().st_size,
                artifact_content_hash="sha256:" + "0" * 64,
            )

            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "content hash mismatch"):
                snapshot.validate_snapshot_artifact(dump_path, manifest)

    @patch("agentic_kb.commands.snapshot._ensure_required_binaries")
    @patch("agentic_kb.commands.snapshot._persist_snapshot_manifest")
    @patch("agentic_kb.commands.snapshot._write_manifest_file")
    @patch("agentic_kb.commands.snapshot.compute_file_sha256")
    @patch("agentic_kb.commands.snapshot._run_subprocess")
    @patch("agentic_kb.commands.snapshot._load_psycopg")
    @patch("agentic_kb.commands.snapshot._utcnow")
    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_export_snapshot_invokes_pg_dump_with_exported_snapshot_and_writes_manifest(
        self,
        from_env,
        utcnow,
        load_psycopg,
        run_subprocess,
        compute_file_sha256,
        write_manifest_file,
        persist_snapshot_manifest,
        ensure_required_binaries,
    ):
        utcnow.return_value = datetime(2026, 3, 29, 2, 0, tzinfo=timezone.utc)
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")
        compute_file_sha256.return_value = "sha256:" + "1" * 64

        cursor = _FakeCursor(
            fetchone_values=[
                (1,),
                (2,),
                (3,),
                (4,),
                (5,),
                (6,),
                (7,),
                ("snapshot-123",),
            ]
        )
        connection = _FakeConnection(cursor)
        load_psycopg.return_value = MagicMock(connect=MagicMock(return_value=connection))

        with patch(
            "agentic_kb.commands.snapshot.PostgresSyncStateStore.list_sync_states",
            return_value=[],
        ):
            with tempfile.TemporaryDirectory() as temp_dir:
                destination = Path(temp_dir) / "kb.dump"
                destination.write_bytes(b"existing")
                result = snapshot.export_snapshot(
                    "postgresql://agentic:agentic@db:5432/agentic_kb",
                    destination,
                )

        ensure_required_binaries.assert_called_once_with("pg_dump")
        run_subprocess.assert_called_once_with(
            [
                "pg_dump",
                "--format=custom",
                "--compress=6",
                "--no-owner",
                "--no-privileges",
                "--schema=agentic",
                f"--file={destination}",
                "--snapshot=snapshot-123",
                "postgresql://agentic:agentic@db:5432/agentic_kb",
            ]
        )
        written_manifest = write_manifest_file.call_args.args[1]
        self.assertEqual(written_manifest["snapshot_name"], "kb")
        self.assertEqual(written_manifest["artifact"]["filename"], "kb.dump")
        self.assertEqual(written_manifest["embedding_model"], "all-minilm:l6-v2")
        persist_snapshot_manifest.assert_called_once()
        self.assertEqual(result.path, destination)
        self.assertEqual(result.manifest_path, Path(temp_dir) / "kb.manifest.json")

    @patch("agentic_kb.commands.snapshot._ensure_required_binaries")
    @patch("agentic_kb.commands.snapshot._persist_snapshot_manifest")
    @patch("agentic_kb.commands.snapshot._load_psycopg")
    def test_export_snapshot_fails_loudly_when_snapshot_export_returns_no_identifier(
        self,
        load_psycopg,
        persist_snapshot_manifest,
        ensure_required_binaries,
    ):
        cursor = _FakeCursor(
            fetchone_values=[
                (1,),
                (2,),
                (3,),
                (4,),
                (5,),
                (6,),
                (7,),
                None,
            ]
        )
        connection = _FakeConnection(cursor)
        load_psycopg.return_value = MagicMock(connect=MagicMock(return_value=connection))

        with patch(
            "agentic_kb.commands.snapshot.PostgresSyncStateStore.list_sync_states",
            return_value=[],
        ), patch("agentic_kb.commands.snapshot.AgenticConfig.from_env", return_value=MagicMock(ollama_embed_model="all-minilm:l6-v2")):
            with tempfile.TemporaryDirectory() as temp_dir:
                destination = Path(temp_dir) / "kb.dump"
                with self.assertRaisesRegex(snapshot.SnapshotCommandError, "failed to export a PostgreSQL snapshot"):
                    snapshot.export_snapshot(
                        "postgresql://agentic:agentic@db:5432/agentic_kb",
                        destination,
                    )
                self.assertFalse(destination.exists())
                self.assertFalse((Path(temp_dir) / "kb.manifest.json").exists())

        ensure_required_binaries.assert_called_once_with("pg_dump")
        persist_snapshot_manifest.assert_not_called()

    @patch("agentic_kb.commands.snapshot._ensure_required_binaries")
    @patch("agentic_kb.commands.snapshot._persist_snapshot_manifest")
    @patch("agentic_kb.commands.snapshot.validate_snapshot_artifact")
    @patch("agentic_kb.commands.snapshot.build_agentic_restore_list")
    @patch("agentic_kb.commands.snapshot._run_subprocess")
    def test_import_snapshot_drops_schema_then_restores_after_validation(
        self,
        run_subprocess,
        build_agentic_restore_list,
        validate_snapshot_artifact,
        persist_snapshot_manifest,
        ensure_required_binaries,
    ):
        with tempfile.TemporaryDirectory() as temp_dir:
            dump_path = Path(temp_dir) / "snapshot.dump"
            dump_path.write_bytes(b"dump-bytes")
            manifest_path = Path(temp_dir) / "snapshot.manifest.json"
            manifest = _manifest_fixture(
                artifact_filename=dump_path.name,
                artifact_size_bytes=dump_path.stat().st_size,
                artifact_content_hash=snapshot.compute_file_sha256(dump_path),
            )
            manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

            restore_list = Path(temp_dir) / "restore.list"
            restore_list.touch()
            build_agentic_restore_list.return_value = restore_list

            result = snapshot.import_snapshot(
                "postgresql://agentic:agentic@db:5432/agentic_kb",
                dump_path,
                confirmed=True,
            )

        ensure_required_binaries.assert_called_once_with("psql", "pg_restore")
        validate_snapshot_artifact.assert_called_once_with(dump_path, manifest)
        self.assertEqual(
            run_subprocess.call_args_list,
            [
                unittest.mock.call(
                    [
                        "psql",
                        "postgresql://agentic:agentic@db:5432/agentic_kb",
                        "--set",
                        "ON_ERROR_STOP=1",
                        "--command",
                        "DROP SCHEMA IF EXISTS agentic CASCADE;",
                    ]
                ),
                unittest.mock.call(
                    [
                        "pg_restore",
                        "--dbname",
                        "postgresql://agentic:agentic@db:5432/agentic_kb",
                        "--exit-on-error",
                        "--no-owner",
                        "--no-privileges",
                        f"--use-list={restore_list}",
                        str(dump_path),
                    ]
                ),
            ],
        )
        persist_snapshot_manifest.assert_called_once()
        self.assertEqual(result.path, dump_path)
        self.assertEqual(result.manifest_path, manifest_path)
        self.assertFalse(restore_list.exists())

    def test_import_snapshot_requires_explicit_confirmation(self):
        with tempfile.NamedTemporaryFile(suffix=".dump") as temp_file:
            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "explicit destructive confirmation"):
                snapshot.import_snapshot(
                    "postgresql://agentic:agentic@db:5432/agentic_kb",
                    temp_file.name,
                    confirmed=False,
                )

    def test_build_pg_restore_command_uses_filtered_restore_list(self):
        command = snapshot.build_pg_restore_command(
            "postgresql://agentic:agentic@db:5432/agentic_kb",
            Path("/tmp/agentic.dump"),
            Path("/tmp/agentic.list"),
        )

        self.assertEqual(
            command,
            [
                "pg_restore",
                "--dbname",
                "postgresql://agentic:agentic@db:5432/agentic_kb",
                "--exit-on-error",
                "--no-owner",
                "--no-privileges",
                "--use-list=/tmp/agentic.list",
                "/tmp/agentic.dump",
            ],
        )

    def test_filter_agentic_restore_toc_keeps_only_agentic_entries(self):
        toc_text = "\n".join(
            [
                "; header",
                "19; 2615 25082 SCHEMA - agentic agentic",
                "14; 2615 22202 SCHEMA - paradedb agentic",
                "303; 1259 25096 TABLE agentic kb_documents agentic",
                "5563; 0 22210 TABLE DATA paradedb _typmod_cache agentic",
                "5731; 1259 25329 INDEX agentic kb_documents_bm25_idx agentic",
            ]
        )

        filtered = snapshot.filter_agentic_restore_toc(toc_text)

        self.assertEqual(
            filtered,
            [
                "; header",
                "19; 2615 25082 SCHEMA - agentic agentic",
                "303; 1259 25096 TABLE agentic kb_documents agentic",
                "5731; 1259 25329 INDEX agentic kb_documents_bm25_idx agentic",
            ],
        )

    def test_filter_agentic_restore_toc_rejects_dump_without_agentic_entries(self):
        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "does not contain any restoreable agentic schema entries"):
            snapshot.filter_agentic_restore_toc("; header\n14; 2615 22202 SCHEMA - paradedb agentic")


class _FakeCursor:
    def __init__(self, fetchone_values):
        self.fetchone_values = list(fetchone_values)
        self.executed = []

    def execute(self, query, params=None):
        self.executed.append((query, params))

    def fetchone(self):
        if not self.fetchone_values:
            return None
        return self.fetchone_values.pop(0)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


class _FakeConnection:
    def __init__(self, cursor):
        self._cursor = cursor
        self.closed = False
        self.autocommit = False

    def cursor(self):
        return self._cursor

    def rollback(self):
        return None

    def close(self):
        self.closed = True


def _manifest_fixture(
    *,
    artifact_filename: str,
    artifact_size_bytes: int,
    artifact_content_hash: str,
):
    return {
        "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
        "schema_version": 1,
        "snapshot_name": "snapshot",
        "snapshot_created_at": "2026-03-29T02:00:00Z",
        "artifact": {
            "filename": artifact_filename,
            "dump_format": "postgresql_custom",
            "compression": {"algorithm": "gzip", "level": 6},
            "size_bytes": artifact_size_bytes,
            "content_hash": artifact_content_hash,
        },
        "repo": {
            "name": "DripDropz/daedalus",
            "docs_commit_hash": "0123456789abcdef0123456789abcdef01234567",
            "code_commit_hash": "0123456789abcdef0123456789abcdef01234567",
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
            "docs": {"repo_commit_hash": "0123456789abcdef0", "last_synced_at": None},
            "code": {"repo_commit_hash": "0123456789abcdef0", "last_synced_at": None},
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


if __name__ == "__main__":
    unittest.main()
