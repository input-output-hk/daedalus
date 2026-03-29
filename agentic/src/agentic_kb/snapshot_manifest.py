from __future__ import annotations

import json
from datetime import datetime, timezone
from functools import lru_cache
from importlib.resources import files
from typing import Any, Iterable

from jsonschema import Draft202012Validator

from agentic_kb.sync.state import (
    DEFAULT_PROJECT_NUMBER,
    DEFAULT_PROJECT_OWNER,
    DEFAULT_SYNC_REPO,
    GITHUB_STREAM_NAMES,
    SyncStateRecord,
    github_scope_key,
    project_scope_key,
    repo_scope_key,
)


SNAPSHOT_MANIFEST_SCHEMA_VERSION = 1
SNAPSHOT_MANIFEST_SCHEMA_ID = (
    "https://raw.githubusercontent.com/DripDropz/daedalus/develop/agentic/config/"
    "snapshot-manifest.schema.json"
)
SNAPSHOT_ARTIFACT_DUMP_FORMAT = "postgresql_custom"
SNAPSHOT_ARTIFACT_COMPRESSION_ALGORITHM = "gzip"
SNAPSHOT_ARTIFACT_COMPRESSION_LEVEL = 6
SNAPSHOT_ARTIFACT_CONTENT_HASH_PREFIX = "sha256:"
SNAPSHOT_ENTITY_COUNT_KEYS: tuple[str, ...] = (
    "documents",
    "code_chunks",
    "github_issues",
    "github_issue_comments",
    "github_prs",
    "github_pr_comments",
    "project_items",
)


class SnapshotManifestValidationError(ValueError):
    pass


def build_snapshot_manifest(
    *,
    snapshot_name: str,
    snapshot_created_at: datetime,
    artifact_filename: str,
    artifact_size_bytes: int,
    artifact_content_hash: str,
    repo_name: str,
    docs_commit_hash: str | None,
    code_commit_hash: str | None,
    embedding_model: str,
    entity_counts: dict[str, int],
    sync_state: dict[str, Any],
) -> dict[str, Any]:
    manifest = {
        "$schema": SNAPSHOT_MANIFEST_SCHEMA_ID,
        "schema_version": SNAPSHOT_MANIFEST_SCHEMA_VERSION,
        "snapshot_name": snapshot_name,
        "snapshot_created_at": format_manifest_timestamp(snapshot_created_at),
        "artifact": {
            "filename": artifact_filename,
            "dump_format": SNAPSHOT_ARTIFACT_DUMP_FORMAT,
            "compression": {
                "algorithm": SNAPSHOT_ARTIFACT_COMPRESSION_ALGORITHM,
                "level": SNAPSHOT_ARTIFACT_COMPRESSION_LEVEL,
            },
            "size_bytes": int(artifact_size_bytes),
            "content_hash": artifact_content_hash,
        },
        "repo": {
            "name": repo_name,
            "docs_commit_hash": docs_commit_hash,
            "code_commit_hash": code_commit_hash,
        },
        "embedding_model": embedding_model,
        "entity_counts": {key: int(entity_counts[key]) for key in SNAPSHOT_ENTITY_COUNT_KEYS},
        "sync_state": sync_state,
    }
    validate_snapshot_manifest(manifest)
    return manifest


def validate_snapshot_manifest(manifest: dict[str, Any]) -> None:
    errors = sorted(
        _snapshot_manifest_validator().iter_errors(manifest),
        key=lambda error: list(error.absolute_path),
    )
    if not errors:
        return

    error = errors[0]
    path = "/".join(str(part) for part in error.absolute_path) or "<root>"
    raise SnapshotManifestValidationError(f"invalid snapshot manifest at {path}: {error.message}")


def snapshot_manifest_record_fields(
    manifest: dict[str, Any],
    *,
    source_path: str,
    imported_at: datetime | None = None,
) -> dict[str, Any]:
    github_state = manifest["sync_state"]["github"]
    return {
        "id": snapshot_manifest_record_id(
            manifest["snapshot_name"],
            manifest["snapshot_created_at"],
        ),
        "snapshot_name": manifest["snapshot_name"],
        "schema_version": int(manifest["schema_version"]),
        "snapshot_created_at": parse_manifest_timestamp(manifest["snapshot_created_at"]),
        "repo_commit_hash": shared_repo_commit_hash(manifest["repo"]),
        "embedding_model": manifest["embedding_model"],
        "entity_counts": manifest["entity_counts"],
        "github_watermarks": {
            stream_name: github_state[stream_name]["updated_at_watermark"]
            for stream_name in GITHUB_STREAM_NAMES
        },
        "manifest": manifest,
        "source_path": source_path,
        "content_hash": manifest["artifact"]["content_hash"],
        "imported_at": imported_at,
    }


def snapshot_manifest_record_id(snapshot_name: str, snapshot_created_at: str) -> str:
    return f"snapshot-manifest:{snapshot_name}:{snapshot_created_at}"


def shared_repo_commit_hash(repo_manifest: dict[str, Any]) -> str | None:
    docs_commit_hash = repo_manifest.get("docs_commit_hash")
    code_commit_hash = repo_manifest.get("code_commit_hash")
    if docs_commit_hash and docs_commit_hash == code_commit_hash:
        return docs_commit_hash
    return None


def parse_manifest_timestamp(value: str) -> datetime:
    return datetime.fromisoformat(value.replace("Z", "+00:00")).astimezone(timezone.utc)


def format_manifest_timestamp(value: datetime) -> str:
    return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def normalize_sync_state_records(
    records: Iterable[SyncStateRecord],
    *,
    repo: str = DEFAULT_SYNC_REPO,
    project_owner: str = DEFAULT_PROJECT_OWNER,
    project_number: int = DEFAULT_PROJECT_NUMBER,
) -> dict[str, Any]:
    records_by_key = {(record.source_name, record.scope_key): record for record in records}

    docs_record = records_by_key.get(("docs", repo_scope_key(repo)))
    code_record = records_by_key.get(("code", repo_scope_key(repo)))
    project_record = records_by_key.get(("project", project_scope_key(project_owner, project_number)))

    return {
        "docs": {
            "repo_commit_hash": _nullable_string(docs_record, "repo_commit_hash"),
            "last_synced_at": _format_manifest_timestamp(
                docs_record.last_succeeded_at if docs_record is not None else None
            ),
        },
        "code": {
            "repo_commit_hash": _nullable_string(code_record, "repo_commit_hash"),
            "last_synced_at": _format_manifest_timestamp(
                code_record.last_succeeded_at if code_record is not None else None
            ),
        },
        "github": {
            stream_name: {
                "updated_at_watermark": _format_manifest_timestamp(
                    _github_stream_record(records_by_key, repo, stream_name)
                )
            }
            for stream_name in GITHUB_STREAM_NAMES
        },
        "project": {
            "owner": project_owner,
            "number": project_number,
            "cursor": _nullable_string(project_record, "cursor_text"),
            "updated_at_watermark": _format_manifest_timestamp(
                project_record.watermark_timestamp if project_record is not None else None
            ),
        },
    }


def _github_stream_record(
    records_by_key: dict[tuple[str, str], SyncStateRecord],
    repo: str,
    stream_name: str,
) -> datetime | None:
    record = records_by_key.get(("github", github_scope_key(stream_name, repo=repo)))
    if record is None:
        return None
    return record.watermark_timestamp


def _nullable_string(record: SyncStateRecord | None, attribute: str) -> str | None:
    if record is None:
        return None
    value = getattr(record, attribute)
    if value is None:
        return None
    return str(value)


def _format_manifest_timestamp(value: datetime | None) -> str | None:
    if value is None:
        return None
    return format_manifest_timestamp(value)


@lru_cache(maxsize=1)
def _snapshot_manifest_validator() -> Draft202012Validator:
    schema_path = files("agentic_kb").joinpath("config", "snapshot-manifest.schema.json")
    with schema_path.open("r", encoding="utf-8") as handle:
        schema = json.load(handle)
    return Draft202012Validator(schema)
