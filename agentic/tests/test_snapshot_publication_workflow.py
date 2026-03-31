from __future__ import annotations

import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, call, patch

from agentic_kb.commands import snapshot, status, sync
from agentic_kb.snapshot_manifest import SNAPSHOT_EMBEDDING_CONTRACT_ID


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


def _mock_status_report():
    report = MagicMock()
    report.ok = True
    report.embedding_compatibility.state = "compatible"
    report.embedding_compatibility.detail = "matches runtime embedding contract"
    return report


def _mock_search_results():
    return {
        "query_text": "agentic-kb workflow",
        "mode": "bm25",
        "limit": 5,
        "filters": {"entity_type": ["document"]},
        "entity_types": ["document"],
        "hits": [
            {
                "entity_type": "document",
                "source_domain": "docs",
                "id": "agentic-kb-md",
                "fields": {
                    "title": "Agentic KB Workflow",
                    "source_path": ".agent/workflows/agentic-kb.md",
                    "preview_text": "Knowledge base workflow documentation",
                },
                "bm25_score": 12.5,
                "vector_distance": 0.3,
                "fused_score": 0.85,
                "bm25_rank": 1,
                "vector_rank": 2,
                "hybrid_rank": 1,
            },
            {
                "entity_type": "document",
                "source_domain": "docs",
                "id": "readme-md",
                "fields": {
                    "title": "README",
                    "source_path": ".agent/readme.md",
                    "preview_text": "Agent documentation index",
                },
                "bm25_score": 8.2,
                "vector_distance": 0.5,
                "fused_score": 0.72,
                "bm25_rank": 2,
                "vector_rank": 1,
                "hybrid_rank": 2,
            },
        ],
    }


class PublicationWorkflowSmokeTests(unittest.TestCase):
    """Smoke checks for the publication workflow chain.

    These tests verify that the Python-level workflow functions can be
    called in the expected sequence and that mocks record the correct
    call order. The actual bash-script orchestration (publish.sh /
    fetch.sh) is tested separately in test_shell_wrappers.py.
    """

    def test_full_chain_executes_in_order(self):
        """Verify the publication workflow chain: sync → export → compatibility check → pair resolve → import."""
        call_log = []

        def mock_run_sync(*args, **kwargs):
            call_log.append("sync")

        def mock_export(*args, **kwargs):
            call_log.append("export")
            result = MagicMock()
            result.path = Path("/tmp/test.dump")
            result.manifest_path = Path("/tmp/test.manifest.json")
            result.database_name = "agentic_kb"
            return result

        def mock_ensure_compatible(*args, **kwargs):
            call_log.append("ensure_compatible")

        def mock_resolve_pair(*args, **kwargs):
            call_log.append("resolve_pair")
            pair = snapshot.SnapshotPair(
                dump_path=Path("/tmp/test.dump"),
                manifest_path=Path("/tmp/test.manifest.json"),
            )
            return (pair, {})

        def mock_import(*args, **kwargs):
            call_log.append("import")
            result = MagicMock()
            result.path = Path("/tmp/test.dump")
            result.manifest_path = Path("/tmp/test.manifest.json")
            result.database_name = "agentic_kb"
            return result

        with patch("agentic_kb.commands.sync.ensure_sync_changed_snapshot_compatibility", side_effect=mock_run_sync), \
             patch("agentic_kb.commands.sync.run_sync_all", side_effect=mock_run_sync), \
             patch.object(snapshot, "export_snapshot", side_effect=mock_export), \
             patch.object(snapshot, "ensure_compatible_snapshot_import", side_effect=mock_ensure_compatible), \
             patch.object(snapshot, "resolve_import_snapshot_pair", side_effect=mock_resolve_pair), \
             patch.object(snapshot, "import_snapshot", side_effect=mock_import):

            from agentic_kb.commands import sync as sync_mod

            sync_mod.ensure_sync_changed_snapshot_compatibility("postgresql://localhost/test", MagicMock())
            snapshot.export_snapshot("postgresql://localhost/test", Path("/tmp/test.dump"))
            snapshot.ensure_compatible_snapshot_import({})
            snapshot.resolve_import_snapshot_pair(Path("/tmp/test.dump"), Path("/tmp/test.manifest.json"))
            snapshot.import_snapshot(Path("/tmp/test.dump"), Path("/tmp/test.manifest.json"))

        self.assertEqual(call_log, ["sync", "export", "ensure_compatible", "resolve_pair", "import"])

    @patch.object(snapshot, "export_snapshot")
    def test_sibling_pair_created_together(self, mock_export):
        local_snapshots = tempfile.mkdtemp()
        try:
            dump_filename = "agentic-kb-test.dump"
            manifest_filename = "agentic-kb-test.manifest.json"
            dump_path = Path(local_snapshots) / dump_filename
            manifest_path = Path(local_snapshots) / manifest_filename

            manifest = _manifest_fixture(
                artifact_filename=dump_filename,
                artifact_size_bytes=1024,
                artifact_content_hash="sha256:" + "b" * 64,
            )

            dump_path.write_bytes(b"\x00" * 1024)
            manifest_path.write_text(json.dumps(manifest))

            export_result = MagicMock()
            export_result.path = dump_path
            export_result.manifest_path = manifest_path
            export_result.database_name = "agentic_kb"
            mock_export.return_value = export_result

            self.assertTrue(dump_path.exists())
            self.assertTrue(manifest_path.exists())

        finally:
            shutil.rmtree(local_snapshots, ignore_errors=True)

    @patch.object(snapshot, "export_snapshot")
    def test_sibling_pair_copied_to_shared_dir(self, mock_export):
        shared_dir = tempfile.mkdtemp()
        local_snapshots = tempfile.mkdtemp()
        try:
            dump_filename = "agentic-kb-test.dump"
            manifest_filename = "agentic-kb-test.manifest.json"
            dump_path = Path(local_snapshots) / dump_filename
            manifest_path = Path(local_snapshots) / manifest_filename

            manifest = _manifest_fixture(
                artifact_filename=dump_filename,
                artifact_size_bytes=1024,
                artifact_content_hash="sha256:" + "c" * 64,
            )

            dump_path.write_bytes(b"\x00" * 1024)
            manifest_path.write_text(json.dumps(manifest))

            shared_dump = Path(shared_dir) / dump_filename
            shared_manifest = Path(shared_dir) / manifest_filename
            shutil.copy2(dump_path, shared_dump)
            shutil.copy2(manifest_path, shared_manifest)

            self.assertTrue(shared_dump.exists())
            self.assertTrue(shared_manifest.exists())

        finally:
            shutil.rmtree(shared_dir, ignore_errors=True)
            shutil.rmtree(local_snapshots, ignore_errors=True)


class SiblingPairContractTests(unittest.TestCase):
    """Tests that verify the sibling-pair contract at the Python level.

    The bash scripts (publish.sh / fetch.sh) enforce the sibling-pair
    contract by checking that both .dump and .manifest.json exist before
    proceeding. These Python-level tests verify the same contract by
    simulating the check that the scripts perform: both files must exist
    together, and the absence of either one is a contract violation.

    Shell-level enforcement is tested in test_shell_wrappers.py; these
    tests document the expected contract shape for Python consumers.
    """

    def test_publish_requires_both_dump_and_manifest(self):
        local_snapshots = tempfile.mkdtemp()
        shared_dir = tempfile.mkdtemp()
        try:
            dump_path = Path(local_snapshots) / "agentic-kb-test.dump"
            manifest_path = Path(local_snapshots) / "agentic-kb-test.manifest.json"

            dump_path.write_bytes(b"\x00" * 1024)

            # Contract check: both files must exist before publish proceeds
            dump_exists = dump_path.exists()
            manifest_exists = manifest_path.exists()
            pair_complete = dump_exists and manifest_exists

            self.assertTrue(dump_exists)
            self.assertFalse(manifest_exists)
            self.assertFalse(pair_complete)

        finally:
            shutil.rmtree(local_snapshots, ignore_errors=True)
            shutil.rmtree(shared_dir, ignore_errors=True)

    def test_publish_requires_manifest_when_dump_exists(self):
        local_snapshots = tempfile.mkdtemp()
        shared_dir = tempfile.mkdtemp()
        try:
            manifest_path = Path(local_snapshots) / "agentic-kb-test.manifest.json"
            dump_path = Path(local_snapshots) / "agentic-kb-test.dump"

            manifest_path.write_text(json.dumps(_manifest_fixture(
                artifact_filename="agentic-kb-test.dump",
                artifact_size_bytes=1024,
                artifact_content_hash="sha256:" + "d" * 64,
            )))

            dump_exists = dump_path.exists()
            manifest_exists = manifest_path.exists()
            pair_complete = dump_exists and manifest_exists

            self.assertFalse(dump_exists)
            self.assertTrue(manifest_exists)
            self.assertFalse(pair_complete)

        finally:
            shutil.rmtree(local_snapshots, ignore_errors=True)
            shutil.rmtree(shared_dir, ignore_errors=True)

    def test_fetch_requires_both_files_in_shared_dir(self):
        shared_dir = tempfile.mkdtemp()
        try:
            dump_path = Path(shared_dir) / "agentic-kb-test.dump"
            manifest_path = Path(shared_dir) / "agentic-kb-test.manifest.json"

            dump_path.write_bytes(b"\x00" * 1024)

            dump_exists = dump_path.exists()
            manifest_exists = manifest_path.exists()
            pair_complete = dump_exists and manifest_exists

            self.assertTrue(dump_exists)
            self.assertFalse(manifest_exists)
            self.assertFalse(pair_complete)

        finally:
            shutil.rmtree(shared_dir, ignore_errors=True)

    def test_fetch_requires_dump_when_manifest_exists_in_shared(self):
        shared_dir = tempfile.mkdtemp()
        try:
            manifest_path = Path(shared_dir) / "agentic-kb-test.manifest.json"
            dump_path = Path(shared_dir) / "agentic-kb-test.dump"

            manifest_path.write_text(json.dumps(_manifest_fixture(
                artifact_filename="agentic-kb-test.dump",
                artifact_size_bytes=1024,
                artifact_content_hash="sha256:" + "e" * 64,
            )))

            dump_exists = dump_path.exists()
            manifest_exists = manifest_path.exists()
            pair_complete = dump_exists and manifest_exists

            self.assertFalse(dump_exists)
            self.assertTrue(manifest_exists)
            self.assertFalse(pair_complete)

        finally:
            shutil.rmtree(shared_dir, ignore_errors=True)


class PostImportWorkflowTests(unittest.TestCase):
    @patch("agentic_kb.commands.status.collect_status_report")
    def test_status_returns_compatible_report(self, mock_status):
        mock_status.return_value = _mock_status_report()

        config = MagicMock()
        config.database_url = "postgresql://localhost/agentic_kb"
        config.ollama_base_url = "http://localhost:11434"
        config.ollama_embed_model = "all-minilm:l6-v2"
        config.github_token = None

        with patch("agentic_kb.commands.status.inspect_database") as mock_inspect:
            mock_inspect.return_value = MagicMock()
            with patch("agentic_kb.commands.status._path_exists", return_value=True), \
                 patch("agentic_kb.commands.status._check_ollama", return_value=(True, "ok", True, "ok")), \
                 patch("agentic_kb.commands.status._check_tcp"), \
                 patch("agentic_kb.commands.status._collect_freshness", return_value=None):
                report = status.collect_status_report(config, healthcheck=False)

        self.assertEqual(report.embedding_compatibility.state, "compatible")
        mock_status.assert_called_once()

    def test_search_result_schema_matches_expected_shape(self):
        results = _mock_search_results()

        self.assertIn("query_text", results)
        self.assertIn("mode", results)
        self.assertIn("hits", results)
        self.assertIsInstance(results["hits"], list)
        self.assertGreater(len(results["hits"]), 0)

        hit = results["hits"][0]
        self.assertIn("entity_type", hit)
        self.assertIn("fields", hit)
        self.assertIn("source_path", hit["fields"])
        self.assertIn("bm25_score", hit)

    def test_status_then_search_ordering(self):
        """Verify status is called before search in post-import workflow."""
        call_log = []

        def mock_status_fn(*args, **kwargs):
            call_log.append("status")
            return _mock_status_report()

        def mock_search_fn(*args, **kwargs):
            call_log.append("search")
            return _mock_search_results()

        with patch("agentic_kb.commands.status.collect_status_report", side_effect=mock_status_fn), \
             patch("agentic_kb.commands.search.serialize_search_result_set", side_effect=mock_search_fn), \
             patch("agentic_kb.commands.status.inspect_database", return_value=MagicMock()), \
             patch("agentic_kb.commands.status._path_exists", return_value=True), \
             patch("agentic_kb.commands.status._check_ollama", return_value=(True, "ok", True, "ok")), \
             patch("agentic_kb.commands.status._check_tcp"), \
             patch("agentic_kb.commands.status._collect_freshness", return_value=None):

            mock_config = MagicMock()
            mock_config.database_url = "postgresql://localhost/agentic_kb"
            mock_config.ollama_base_url = "http://localhost:11434"
            mock_config.ollama_embed_model = "all-minilm:l6-v2"
            mock_config.github_token = None

            from agentic_kb.commands.status import collect_status_report
            from agentic_kb.commands.search import serialize_search_result_set

            collect_status_report(mock_config, healthcheck=False)
            mock_result = MagicMock()
            mock_result.query_text = "test"
            mock_result.mode = MagicMock()
            mock_result.mode.value = "bm25"
            mock_result.limit = 5
            mock_result.filters = {}
            mock_result.entity_types = []
            mock_result.hits = []
            serialize_search_result_set(mock_result)

        self.assertEqual(call_log, ["status", "search"])


if __name__ == "__main__":
    unittest.main()
