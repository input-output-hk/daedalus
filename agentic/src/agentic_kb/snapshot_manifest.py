from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Iterable

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
    return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
