from __future__ import annotations

import json
import os
import unittest
from datetime import datetime, timezone
from importlib.util import find_spec
from pathlib import Path

from agentic_kb.commands import status
from agentic_kb.config import AgenticConfig


TEST_DATABASE_URL_ENV = "AGENTIC_TEST_DATABASE_URL"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


@unittest.skipUnless(
    os.getenv(TEST_DATABASE_URL_ENV) and find_spec("psycopg") is not None,
    "requires AGENTIC_TEST_DATABASE_URL and psycopg",
)
class StatusCommandDbTests(unittest.TestCase):
    def setUp(self):
        import psycopg

        self.database_url = os.environ[TEST_DATABASE_URL_ENV]
        self.connection = psycopg.connect(self.database_url)
        self.addCleanup(self.connection.close)
        _bootstrap_database(self.connection)
        _seed_status_fixture(self.connection)
        self.connection.commit()

    def test_inspect_database_reports_current_schema_indexes_counts_and_sync_state(self):
        inspection = status.inspect_database(self.database_url)

        self.assertEqual(inspection.applied_versions, (1, 2, 3))
        self.assertTrue(set(status.EXPECTED_AGENTIC_TABLES).issubset(set(inspection.tables)))
        self.assertTrue(set(status.expected_searchable_indexes()).issubset(set(inspection.indexes)))
        self.assertEqual(inspection.row_counts["agentic.kb_documents"], 1)
        docs_summary = next(summary for summary in inspection.sync_summaries if summary.source_name == "docs")
        self.assertEqual(docs_summary.row_count, 1)
        self.assertEqual(docs_summary.error_count, 0)

    def test_inspect_database_prefers_latest_imported_snapshot_manifest_over_export_only_rows(self):
        with self.connection.transaction():
            with self.connection.cursor() as cursor:
                cursor.execute("TRUNCATE TABLE agentic.kb_snapshot_manifest")
                legacy_manifest = _manifest_fixture("export-only", contract=None, legacy=True)
                compatible_manifest = _manifest_fixture(
                    "imported-compatible",
                    contract={
                        "contract_id": "daedalus-agentic-kb-embedding-contract-v1",
                        "embedding_model": "all-minilm",
                        "embedding_dimension": 384,
                    }
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_snapshot_manifest (
                        id, snapshot_name, schema_version, snapshot_created_at, embedding_model,
                        entity_counts, github_watermarks, manifest, source_path, content_hash, imported_at
                    ) VALUES (%s, %s, %s, %s, %s, '{}'::jsonb, '{}'::jsonb, %s::jsonb, %s, %s, %s)
                    """,
                    (
                        "export-only",
                        "export-only",
                        1,
                        datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
                        "legacy-model",
                        json.dumps(legacy_manifest),
                        "/tmp/export-only.manifest.json",
                        "sha256:" + "0" * 64,
                        None,
                    ),
                )
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_snapshot_manifest (
                        id, snapshot_name, schema_version, snapshot_created_at, embedding_model,
                        entity_counts, github_watermarks, manifest, source_path, content_hash, imported_at
                    ) VALUES (%s, %s, %s, %s, %s, '{}'::jsonb, '{}'::jsonb, %s::jsonb, %s, %s, %s)
                    """,
                    (
                        "imported-compatible",
                        "imported-compatible",
                        1,
                        datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
                        "all-minilm",
                        json.dumps(compatible_manifest),
                        "/tmp/imported-compatible.manifest.json",
                        "sha256:" + "1" * 64,
                        datetime(2026, 3, 29, 11, 5, tzinfo=timezone.utc),
                    ),
                )

        inspection = status.inspect_database(self.database_url)

        self.assertIsNotNone(inspection.latest_imported_snapshot_manifest)
        self.assertEqual(inspection.latest_imported_snapshot_manifest.snapshot_name, "imported-compatible")

    def test_collect_status_report_reports_compatible_imported_snapshot_contract(self):
        _replace_snapshot_manifests(
            self.connection,
            [
                _snapshot_manifest_row(
                    row_id="imported-compatible",
                    snapshot_name="imported-compatible",
                    snapshot_created_at=datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
                    manifest=_manifest_fixture(
                        snapshot_name="imported-compatible",
                        contract={
                            "contract_id": "daedalus-agentic-kb-embedding-contract-v1",
                            "embedding_model": "all-minilm",
                            "embedding_dimension": 384,
                        },
                    ),
                    embedding_model="all-minilm",
                    imported_at=datetime(2026, 3, 29, 11, 5, tzinfo=timezone.utc),
                )
            ],
        )

        report = status.collect_status_report(
            AgenticConfig(
                database_url=self.database_url,
                ollama_base_url="http://ollama:11434",
                ollama_embed_model="all-minilm",
                github_token=None,
            ),
            healthcheck=False,
        )

        self.assertEqual(report.embedding_compatibility.state, "compatible")

    def test_collect_status_report_reports_incompatible_imported_snapshot_contract(self):
        _replace_snapshot_manifests(
            self.connection,
            [
                _snapshot_manifest_row(
                    row_id="imported-incompatible",
                    snapshot_name="imported-incompatible",
                    snapshot_created_at=datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
                    manifest=_manifest_fixture(
                        snapshot_name="imported-incompatible",
                        contract={
                            "contract_id": "daedalus-agentic-kb-embedding-contract-v1",
                            "embedding_model": "different-model",
                            "embedding_dimension": 384,
                        },
                    ),
                    embedding_model="different-model",
                    imported_at=datetime(2026, 3, 29, 11, 5, tzinfo=timezone.utc),
                )
            ],
        )

        report = status.collect_status_report(
            AgenticConfig(
                database_url=self.database_url,
                ollama_base_url="http://ollama:11434",
                ollama_embed_model="all-minilm",
                github_token=None,
            ),
            healthcheck=False,
        )

        self.assertEqual(report.embedding_compatibility.state, "incompatible")
        self.assertIn("embedding model", report.embedding_compatibility.detail)

    def test_collect_status_report_reports_legacy_imported_snapshot_manifest(self):
        _replace_snapshot_manifests(
            self.connection,
            [
                _snapshot_manifest_row(
                    row_id="imported-legacy",
                    snapshot_name="imported-legacy",
                    snapshot_created_at=datetime(2026, 3, 29, 11, 0, tzinfo=timezone.utc),
                    manifest=_manifest_fixture(
                        snapshot_name="imported-legacy",
                        contract=None,
                        legacy=True,
                    ),
                    embedding_model="legacy-model",
                    imported_at=datetime(2026, 3, 29, 11, 5, tzinfo=timezone.utc),
                )
            ],
        )

        report = status.collect_status_report(
            AgenticConfig(
                database_url=self.database_url,
                ollama_base_url="http://ollama:11434",
                ollama_embed_model="all-minilm",
                github_token=None,
            ),
            healthcheck=False,
        )

        self.assertEqual(report.embedding_compatibility.state, "unsupported_legacy_manifest")
        self.assertIn("legacy embedding_model-only manifests", report.embedding_compatibility.detail)


def _manifest_fixture(snapshot_name: str, contract, *, legacy=False):
    manifest = {
        "$schema": "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/snapshot-manifest.schema.json",
        "schema_version": 1,
        "snapshot_name": snapshot_name,
        "snapshot_created_at": "2026-03-29T10:00:00Z",
        "artifact": {
            "filename": "fixture.dump",
            "dump_format": "postgresql_custom",
            "compression": {"algorithm": "gzip", "level": 6},
            "size_bytes": 1,
            "content_hash": "sha256:" + "2" * 64,
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
        manifest["embedding_model"] = "legacy-model"
    else:
        manifest["embedding_contract"] = contract
    return manifest


def _snapshot_manifest_row(
    *,
    row_id: str,
    snapshot_name: str,
    snapshot_created_at: datetime,
    manifest: dict,
    embedding_model: str,
    imported_at: datetime | None,
):
    return {
        "id": row_id,
        "snapshot_name": snapshot_name,
        "schema_version": 1,
        "snapshot_created_at": snapshot_created_at,
        "embedding_model": embedding_model,
        "manifest": json.dumps(manifest),
        "source_path": f"/tmp/{snapshot_name}.manifest.json",
        "content_hash": "sha256:" + "3" * 64,
        "imported_at": imported_at,
    }


def _replace_snapshot_manifests(connection, rows) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("TRUNCATE TABLE agentic.kb_snapshot_manifest")
            for row in rows:
                cursor.execute(
                    """
                    INSERT INTO agentic.kb_snapshot_manifest (
                        id, snapshot_name, schema_version, snapshot_created_at, embedding_model,
                        entity_counts, github_watermarks, manifest, source_path, content_hash, imported_at
                    ) VALUES (%(id)s, %(snapshot_name)s, %(schema_version)s, %(snapshot_created_at)s, %(embedding_model)s,
                              '{}'::jsonb, '{}'::jsonb, %(manifest)s::jsonb, %(source_path)s, %(content_hash)s, %(imported_at)s)
                    """,
                    row,
                )


def _bootstrap_database(connection) -> None:
    init_sql = _sanitized_sql(SCHEMA_DIR / "init.sql")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def _seed_status_fixture(connection) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("TRUNCATE TABLE agentic.kb_sync_state, agentic.kb_documents CASCADE")
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
                    'docs:status-db#0',
                    'docs',
                    'workflow',
                    '.agent/workflows/agentic-kb.md',
                    'status workflow',
                    '[]'::jsonb,
                    0,
                    'workflow content',
                    'workflow content',
                    'status-doc-hash'
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
                    'sync-state:docs:repo:DripDropz/daedalus:status-db',
                    'docs',
                    'repo:DripDropz/daedalus:status-db',
                    'deadbeef',
                    %s,
                    %s,
                    '{"repo": "DripDropz/daedalus"}'::jsonb
                )
                """,
                (
                    datetime(2026, 3, 28, 23, 10, tzinfo=timezone.utc),
                    datetime(2026, 3, 28, 23, 11, tzinfo=timezone.utc),
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
