from __future__ import annotations

import unittest
from datetime import datetime, timezone
from unittest.mock import MagicMock, patch

from agentic_kb.commands import snapshot, status, sync
from agentic_kb.config import AgenticConfig
from agentic_kb.snapshot_manifest import SNAPSHOT_EMBEDDING_CONTRACT_ID, SnapshotManifestRecord


class SnapshotImportCompatibilityTests(unittest.TestCase):
    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_import_snapshot_rejects_legacy_manifest(self, from_env):
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")

        manifest = _manifest_fixture(
            artifact_filename="snapshot.dump",
            artifact_size_bytes=1,
            artifact_content_hash="sha256:" + "0" * 64,
        )
        manifest["embedding_model"] = manifest["embedding_contract"]["embedding_model"]
        del manifest["embedding_contract"]

        with self.assertRaisesRegex(
            snapshot.SnapshotCommandError,
            "legacy embedding_model-only manifests are unsupported",
        ):
            snapshot.ensure_compatible_snapshot_import(manifest)

    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_import_snapshot_rejects_embedding_dimension_mismatch(self, from_env):
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")

        manifest = _manifest_fixture(
            artifact_filename="snapshot.dump",
            artifact_size_bytes=1,
            artifact_content_hash="sha256:" + "0" * 64,
        )
        manifest["embedding_contract"]["embedding_dimension"] = 512

        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "vector dimensionality"):
            snapshot.ensure_compatible_snapshot_import(manifest)

    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_import_snapshot_rejects_embedding_model_mismatch(self, from_env):
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")

        manifest = _manifest_fixture(
            artifact_filename="snapshot.dump",
            artifact_size_bytes=1,
            artifact_content_hash="sha256:" + "0" * 64,
        )
        manifest["embedding_contract"]["embedding_model"] = "different-model"

        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "embedding model"):
            snapshot.ensure_compatible_snapshot_import(manifest)

    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_import_snapshot_rejects_contract_identifier_mismatch(self, from_env):
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")

        manifest = _manifest_fixture(
            artifact_filename="snapshot.dump",
            artifact_size_bytes=1,
            artifact_content_hash="sha256:" + "0" * 64,
        )
        manifest["embedding_contract"]["contract_id"] = "older-contract-v0"

        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "contract identifier"):
            snapshot.ensure_compatible_snapshot_import(manifest)

    @patch("agentic_kb.commands.snapshot.AgenticConfig.from_env")
    def test_import_snapshot_rejects_embedding_contract_extra_keys(self, from_env):
        from_env.return_value = MagicMock(ollama_embed_model="all-minilm:l6-v2")

        manifest = _manifest_fixture(
            artifact_filename="snapshot.dump",
            artifact_size_bytes=1,
            artifact_content_hash="sha256:" + "0" * 64,
        )
        manifest["embedding_contract"]["unexpected_key"] = "extra-value"

        with self.assertRaisesRegex(snapshot.SnapshotCommandError, "unexpected fields"):
            snapshot.ensure_compatible_snapshot_import(manifest)


class SnapshotStatusCompatibilityTests(unittest.TestCase):
    def setUp(self):
        self.config = AgenticConfig(
            database_url="postgresql://agentic:agentic@db:5432/agentic_kb",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )

    @patch("agentic_kb.commands.status.inspect_database")
    def test_status_report_compatible_when_contracts_match(
        self, inspect_database
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
            latest_imported_snapshot_manifest=_snapshot_record(
                {
                    "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                    "embedding_model": "all-minilm",
                    "embedding_dimension": 384,
                }
            ),
        )

        with patch("agentic_kb.commands.status._path_exists", return_value=True), patch(
            "agentic_kb.commands.status._check_ollama",
            return_value=(True, "reachable", True, "configured model available"),
        ), patch("agentic_kb.commands.status._check_tcp"), patch(
            "agentic_kb.commands.status._collect_freshness",
            return_value=None,
        ):
            report = status.collect_status_report(self.config, healthcheck=False)

        self.assertEqual(report.embedding_compatibility.state, "compatible")
        self.assertIn("matches", report.embedding_compatibility.detail)

    @patch("agentic_kb.commands.status.inspect_database")
    def test_status_report_incompatible_when_embedding_model_differs(
        self, inspect_database
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
            latest_imported_snapshot_manifest=_snapshot_record(
                {
                    "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                    "embedding_model": "different-model",
                    "embedding_dimension": 384,
                }
            ),
        )

        with patch("agentic_kb.commands.status._path_exists", return_value=True), patch(
            "agentic_kb.commands.status._check_ollama",
            return_value=(True, "reachable", True, "configured model available"),
        ), patch("agentic_kb.commands.status._check_tcp"), patch(
            "agentic_kb.commands.status._collect_freshness",
            return_value=None,
        ):
            report = status.collect_status_report(self.config, healthcheck=False)

        self.assertEqual(report.embedding_compatibility.state, "incompatible")
        self.assertIn("embedding model", report.embedding_compatibility.detail)

    @patch("agentic_kb.commands.status.inspect_database")
    def test_status_report_missing_imported_snapshot_metadata(
        self, inspect_database
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
            latest_imported_snapshot_manifest=None,
        )

        with patch("agentic_kb.commands.status._path_exists", return_value=True), patch(
            "agentic_kb.commands.status._check_ollama",
            return_value=(True, "reachable", True, "configured model available"),
        ), patch("agentic_kb.commands.status._check_tcp"), patch(
            "agentic_kb.commands.status._collect_freshness",
            return_value=None,
        ):
            report = status.collect_status_report(self.config, healthcheck=False)

        self.assertEqual(
            report.embedding_compatibility.state, "missing_imported_snapshot_metadata"
        )

    @patch("agentic_kb.commands.status.inspect_database")
    def test_status_report_unsupported_legacy_manifest(self, inspect_database):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
            latest_imported_snapshot_manifest=_snapshot_record(None, legacy=True),
        )

        with patch("agentic_kb.commands.status._path_exists", return_value=True), patch(
            "agentic_kb.commands.status._check_ollama",
            return_value=(True, "reachable", True, "configured model available"),
        ), patch("agentic_kb.commands.status._check_tcp"), patch(
            "agentic_kb.commands.status._collect_freshness",
            return_value=None,
        ):
            report = status.collect_status_report(self.config, healthcheck=False)

        self.assertEqual(
            report.embedding_compatibility.state, "unsupported_legacy_manifest"
        )
        self.assertIn("legacy embedding_model-only manifests", report.embedding_compatibility.detail)

    @patch("agentic_kb.commands.status.inspect_database")
    def test_status_report_unavailable_when_extra_embedding_contract_keys(
        self, inspect_database
    ):
        inspect_database.return_value = status.DatabaseInspection(
            current_database="agentic_kb",
            applied_versions=(1, 2, 3),
            tables=status.EXPECTED_AGENTIC_TABLES,
            indexes=status.expected_searchable_indexes(),
            row_counts={table_name: 0 for table_name in status.expected_searchable_tables()},
            sync_summaries=(),
            latest_imported_snapshot_manifest=_snapshot_record(
                {
                    "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                    "embedding_model": "all-minilm",
                    "embedding_dimension": 384,
                    "unexpected": "extra",
                }
            ),
        )

        with patch("agentic_kb.commands.status._path_exists", return_value=True), patch(
            "agentic_kb.commands.status._check_ollama",
            return_value=(True, "reachable", True, "configured model available"),
        ), patch("agentic_kb.commands.status._check_tcp"), patch(
            "agentic_kb.commands.status._collect_freshness",
            return_value=None,
        ):
            report = status.collect_status_report(self.config, healthcheck=False)

        self.assertEqual(report.embedding_compatibility.state, "unavailable")
        self.assertIn("unexpected fields", report.embedding_compatibility.detail)


class SnapshotSyncCompatibilityTests(unittest.TestCase):
    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_allows_local_only_kb_without_imported_snapshot_metadata(
        self, load_manifest
    ):
        load_manifest.return_value = None

        sync.ensure_sync_changed_snapshot_compatibility(
            "postgresql://localhost/test",
            AgenticConfig(
                database_url="postgresql://localhost/test",
                ollama_base_url="http://ollama:11434",
                ollama_embed_model="all-minilm",
                github_token="token",
            ),
        )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_legacy_imported_manifest(self, load_manifest):
        load_manifest.return_value = _snapshot_record(None, legacy=True)

        with self.assertRaisesRegex(
            sync.SyncCommandError,
            "legacy embedding_model-only manifests are unsupported",
        ):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_incompatible_contract(self, load_manifest):
        load_manifest.return_value = _snapshot_record(
            {
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "different-model",
                "embedding_dimension": 384,
            }
        )

        with self.assertRaisesRegex(sync.SyncCommandError, "embedding model"):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )

    @patch("agentic_kb.commands.sync._load_latest_imported_snapshot_manifest")
    def test_sync_changed_rejects_malformed_metadata_with_extra_keys(
        self, load_manifest
    ):
        load_manifest.return_value = _snapshot_record(
            {
                "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
                "embedding_model": "all-minilm",
                "embedding_dimension": 384,
                "unexpected": True,
            }
        )

        with self.assertRaisesRegex(sync.SyncCommandError, "unexpected fields"):
            sync.ensure_sync_changed_snapshot_compatibility(
                "postgresql://localhost/test",
                AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token="token",
                ),
            )


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
        "embedding_contract": {
            "contract_id": SNAPSHOT_EMBEDDING_CONTRACT_ID,
            "embedding_model": "all-minilm:l6-v2",
            "embedding_dimension": 384,
        },
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


def _snapshot_record(contract, *, legacy=False):
    manifest = {
        "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
        "schema_version": 1,
        "snapshot_name": "imported-snapshot",
        "snapshot_created_at": "2026-03-29T12:00:00Z",
        "artifact": {
            "filename": "imported-snapshot.dump",
            "dump_format": "postgresql_custom",
            "compression": {"algorithm": "gzip", "level": 6},
            "size_bytes": 1,
            "content_hash": "sha256:" + "1" * 64,
        },
        "repo": {"name": "DripDropz/daedalus", "docs_commit_hash": None, "code_commit_hash": None},
        "entity_counts": {
            "documents": 0,
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
            "project": {"owner": "DripDropz", "number": 5, "cursor": None, "updated_at_watermark": None},
        },
    }
    if legacy:
        manifest["embedding_model"] = "all-minilm"
    else:
        manifest["embedding_contract"] = contract
    return SnapshotManifestRecord(
        snapshot_name="imported-snapshot",
        snapshot_created_at=datetime(2026, 3, 29, 12, 0, tzinfo=timezone.utc),
        imported_at=datetime(2026, 3, 29, 12, 5, tzinfo=timezone.utc),
        source_path="/tmp/imported-snapshot.manifest.json",
        content_hash="sha256:" + "1" * 64,
        manifest=manifest,
    )


if __name__ == "__main__":
    unittest.main()
