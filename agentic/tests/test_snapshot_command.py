from __future__ import annotations

import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

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

    def test_resolve_import_path_rejects_missing_file(self):
        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "does not exist"):
            snapshot.resolve_import_path("missing.dump")

    @patch("agentic_kb.commands.snapshot._ensure_required_binaries")
    @patch("agentic_kb.commands.snapshot._run_subprocess")
    def test_export_snapshot_invokes_pg_dump(self, run_subprocess, ensure_required_binaries):
        with tempfile.TemporaryDirectory() as temp_dir:
            destination = Path(temp_dir) / "kb.dump"
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
                "postgresql://agentic:agentic@db:5432/agentic_kb",
            ]
        )
        self.assertEqual(result.path, destination)
        self.assertEqual(result.database_name, "agentic_kb")

    def test_import_snapshot_requires_explicit_confirmation(self):
        with tempfile.NamedTemporaryFile(suffix=".dump") as temp_file:
            with self.assertRaisesRegex(snapshot.SnapshotCommandError, "explicit destructive confirmation"):
                snapshot.import_snapshot(
                    "postgresql://agentic:agentic@db:5432/agentic_kb",
                    temp_file.name,
                    confirmed=False,
                )

    @patch("agentic_kb.commands.snapshot._ensure_required_binaries")
    @patch("agentic_kb.commands.snapshot.build_agentic_restore_list")
    @patch("agentic_kb.commands.snapshot._run_subprocess")
    def test_import_snapshot_drops_schema_then_restores(
        self,
        run_subprocess,
        build_agentic_restore_list,
        ensure_required_binaries,
    ):
        with tempfile.NamedTemporaryFile(suffix=".dump") as temp_file:
            restore_list = Path(temp_file.name + ".list")
            restore_list.touch()
            build_agentic_restore_list.return_value = restore_list
            result = snapshot.import_snapshot(
                "postgresql://agentic:agentic@db:5432/agentic_kb",
                temp_file.name,
                confirmed=True,
            )

        ensure_required_binaries.assert_called_once_with("psql", "pg_restore")
        build_agentic_restore_list.assert_called_once_with(Path(temp_file.name))
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
                        temp_file.name,
                    ]
                ),
            ],
        )
        self.assertEqual(result.path, Path(temp_file.name))
        self.assertFalse(restore_list.exists())

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


if __name__ == "__main__":
    unittest.main()
