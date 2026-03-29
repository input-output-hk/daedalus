from __future__ import annotations

import copy
import json
import unittest
from datetime import datetime, timezone
from pathlib import Path

from jsonschema import Draft202012Validator

from agentic_kb.snapshot_manifest import (
    SNAPSHOT_ARTIFACT_COMPRESSION_ALGORITHM,
    SNAPSHOT_ARTIFACT_COMPRESSION_LEVEL,
    SNAPSHOT_ARTIFACT_CONTENT_HASH_PREFIX,
    SNAPSHOT_ARTIFACT_DUMP_FORMAT,
    SNAPSHOT_ENTITY_COUNT_KEYS,
    SNAPSHOT_MANIFEST_SCHEMA_ID,
    SNAPSHOT_MANIFEST_SCHEMA_VERSION,
    normalize_sync_state_records,
)
from agentic_kb.sync.state import GITHUB_STREAM_NAMES, SyncStateRecord


CONFIG_DIR = Path(__file__).resolve().parents[1] / "config"
SCHEMA_PATH = CONFIG_DIR / "snapshot-manifest.schema.json"
EXAMPLE_PATH = CONFIG_DIR / "snapshot-manifest.example.json"


class SnapshotManifestSchemaTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.schema = _load_json(SCHEMA_PATH)
        cls.example = _load_json(EXAMPLE_PATH)
        cls.validator = Draft202012Validator(cls.schema)

    def test_schema_metadata_matches_contract_constants(self):
        self.assertEqual(self.schema["$id"], SNAPSHOT_MANIFEST_SCHEMA_ID)
        self.assertEqual(self.example["$schema"], SNAPSHOT_MANIFEST_SCHEMA_ID)
        self.assertEqual(self.example["schema_version"], SNAPSHOT_MANIFEST_SCHEMA_VERSION)
        self.assertEqual(self.example["artifact"]["dump_format"], SNAPSHOT_ARTIFACT_DUMP_FORMAT)
        self.assertEqual(
            self.example["artifact"]["compression"]["algorithm"],
            SNAPSHOT_ARTIFACT_COMPRESSION_ALGORITHM,
        )
        self.assertEqual(
            self.example["artifact"]["compression"]["level"],
            SNAPSHOT_ARTIFACT_COMPRESSION_LEVEL,
        )
        self.assertTrue(self.example["artifact"]["content_hash"].startswith(SNAPSHOT_ARTIFACT_CONTENT_HASH_PREFIX))
        self.assertEqual(tuple(self.example["entity_counts"].keys()), SNAPSHOT_ENTITY_COUNT_KEYS)

    def test_example_manifest_conforms_to_schema(self):
        self._assert_valid(self.example)

    def test_top_level_required_fields_are_enforced(self):
        manifest = copy.deepcopy(self.example)
        del manifest["sync_state"]

        self._assert_invalid(manifest, "")

    def test_artifact_filename_rejects_directory_components(self):
        manifest = copy.deepcopy(self.example)
        manifest["artifact"]["filename"] = "snapshots/latest.dump"

        self._assert_invalid(manifest, "artifact/filename")

    def test_artifact_identity_fields_are_required_and_strict(self):
        manifest = copy.deepcopy(self.example)
        del manifest["artifact"]["size_bytes"]

        self._assert_invalid(manifest, "artifact")

        manifest = copy.deepcopy(self.example)
        manifest["artifact"]["compression"]["algorithm"] = "zstd"
        self._assert_invalid(manifest, "artifact/compression/algorithm")

    def test_content_hash_requires_prefixed_lowercase_sha256_hex(self):
        manifest = copy.deepcopy(self.example)
        manifest["artifact"]["content_hash"] = "sha256:ABCDEF"
        self._assert_invalid(manifest, "artifact/content_hash")

    def test_entity_counts_reject_missing_or_extra_keys(self):
        manifest = copy.deepcopy(self.example)
        del manifest["entity_counts"]["github_pr_comments"]
        self._assert_invalid(manifest, "entity_counts")

        manifest = copy.deepcopy(self.example)
        manifest["entity_counts"]["workflows"] = 2
        self._assert_invalid(manifest, "entity_counts")

    def test_sync_state_rejects_internal_last_error_and_unknown_streams(self):
        manifest = copy.deepcopy(self.example)
        manifest["sync_state"]["docs"]["last_error"] = "transient failure"
        self._assert_invalid(manifest, "sync_state/docs")

        manifest = copy.deepcopy(self.example)
        manifest["sync_state"]["github"]["discussions"] = {"updated_at_watermark": None}
        self._assert_invalid(manifest, "sync_state/github")

    def test_normalize_sync_state_records_exports_public_fields_only(self):
        attempted_at = datetime(2026, 3, 29, 1, 0, tzinfo=timezone.utc)
        succeeded_at = datetime(2026, 3, 29, 1, 5, tzinfo=timezone.utc)

        records = [
            SyncStateRecord(
                id="sync-state:docs:repo:DripDropz/daedalus",
                source_name="docs",
                scope_key="repo:DripDropz/daedalus",
                repo_commit_hash="docs-commit",
                cursor_text=None,
                watermark_text=None,
                watermark_timestamp=None,
                schema_version=None,
                last_attempted_at=attempted_at,
                last_succeeded_at=succeeded_at,
                last_error="should-not-export",
                metadata={"repo": "DripDropz/daedalus"},
                created_at=attempted_at,
                updated_at=succeeded_at,
            ),
            SyncStateRecord(
                id="sync-state:code:repo:DripDropz/daedalus",
                source_name="code",
                scope_key="repo:DripDropz/daedalus",
                repo_commit_hash="code-commit",
                cursor_text=None,
                watermark_text=None,
                watermark_timestamp=None,
                schema_version=None,
                last_attempted_at=attempted_at,
                last_succeeded_at=succeeded_at,
                last_error=None,
                metadata={"repo": "DripDropz/daedalus"},
                created_at=attempted_at,
                updated_at=succeeded_at,
            ),
            *[
                SyncStateRecord(
                    id=f"sync-state:github:repo:DripDropz/daedalus:{stream_name}",
                    source_name="github",
                    scope_key=f"repo:DripDropz/daedalus:{stream_name}",
                    repo_commit_hash=None,
                    cursor_text=None,
                    watermark_text="2026-03-29T01:10:00Z",
                    watermark_timestamp=datetime(2026, 3, 29, 1, 10, tzinfo=timezone.utc),
                    schema_version=None,
                    last_attempted_at=attempted_at,
                    last_succeeded_at=succeeded_at,
                    last_error=None,
                    metadata={"stream_name": stream_name},
                    created_at=attempted_at,
                    updated_at=succeeded_at,
                )
                for stream_name in GITHUB_STREAM_NAMES
            ],
            SyncStateRecord(
                id="sync-state:project:project:DripDropz/5",
                source_name="project",
                scope_key="project:DripDropz/5",
                repo_commit_hash=None,
                cursor_text="cursor-after",
                watermark_text="2026-03-29T01:12:00Z",
                watermark_timestamp=datetime(2026, 3, 29, 1, 12, tzinfo=timezone.utc),
                schema_version=None,
                last_attempted_at=attempted_at,
                last_succeeded_at=succeeded_at,
                last_error=None,
                metadata={"project_owner": "DripDropz", "project_number": 5},
                created_at=attempted_at,
                updated_at=succeeded_at,
            ),
        ]

        manifest_sync_state = normalize_sync_state_records(records)

        self.assertEqual(
            manifest_sync_state,
            {
                "docs": {
                    "repo_commit_hash": "docs-commit",
                    "last_synced_at": "2026-03-29T01:05:00Z",
                },
                "code": {
                    "repo_commit_hash": "code-commit",
                    "last_synced_at": "2026-03-29T01:05:00Z",
                },
                "github": {
                    stream_name: {"updated_at_watermark": "2026-03-29T01:10:00Z"}
                    for stream_name in GITHUB_STREAM_NAMES
                },
                "project": {
                    "owner": "DripDropz",
                    "number": 5,
                    "cursor": "cursor-after",
                    "updated_at_watermark": "2026-03-29T01:12:00Z",
                },
            },
        )
        self.assertNotIn("last_error", manifest_sync_state["docs"])

    def _assert_valid(self, manifest):
        errors = sorted(self.validator.iter_errors(manifest), key=lambda error: list(error.absolute_path))
        self.assertEqual(errors, [])

    def _assert_invalid(self, manifest, path_fragment: str):
        errors = list(self.validator.iter_errors(manifest))
        self.assertTrue(errors, "expected manifest validation to fail")
        joined_paths = ["/".join(str(part) for part in error.absolute_path) for error in errors]
        self.assertTrue(
            any(path_fragment in path or path == path_fragment for path in joined_paths),
            f"expected validation error under {path_fragment}, got {joined_paths}",
        )


def _load_json(path: Path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


if __name__ == "__main__":
    unittest.main()
