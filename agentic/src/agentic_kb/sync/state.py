from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Any, Protocol, Sequence


DEFAULT_SYNC_REPO = "DripDropz/daedalus"
DEFAULT_PROJECT_OWNER = "DripDropz"
DEFAULT_PROJECT_NUMBER = 5
GITHUB_STREAM_NAMES: tuple[str, ...] = (
    "issues",
    "pulls",
    "issue_comments",
    "review_comments",
)
MAX_SYNC_ERROR_LENGTH = 1000


@dataclass(frozen=True)
class PreparedSyncAttempt:
    id: str
    source_name: str
    scope_key: str
    attempted_at: datetime
    metadata: dict[str, Any] | None = None


@dataclass(frozen=True)
class PreparedSyncFailure:
    id: str
    source_name: str
    scope_key: str
    attempted_at: datetime
    error: str
    metadata: dict[str, Any] | None = None


@dataclass(frozen=True)
class PreparedSyncState:
    id: str
    source_name: str
    scope_key: str
    repo_commit_hash: str | None
    cursor_text: str | None
    watermark_text: str | None
    watermark_timestamp: datetime | None
    schema_version: int | None
    last_attempted_at: datetime
    last_succeeded_at: datetime | None
    last_error: str | None
    metadata: dict[str, Any]


@dataclass(frozen=True)
class SyncStateRecord:
    id: str
    source_name: str
    scope_key: str
    repo_commit_hash: str | None
    cursor_text: str | None
    watermark_text: str | None
    watermark_timestamp: datetime | None
    schema_version: int | None
    last_attempted_at: datetime | None
    last_succeeded_at: datetime | None
    last_error: str | None
    metadata: dict[str, Any]
    created_at: datetime
    updated_at: datetime


class SyncStateStore(Protocol):
    def record_attempts(self, attempts: Sequence[PreparedSyncAttempt]) -> int:
        ...

    def record_failures(self, failures: Sequence[PreparedSyncFailure]) -> int:
        ...

    def upsert_sync_states(self, states: Sequence[PreparedSyncState]) -> int:
        ...

    def get_sync_state(self, source_name: str, scope_key: str) -> SyncStateRecord | None:
        ...

    def list_sync_states(self, *, source_name: str | None = None) -> list[SyncStateRecord]:
        ...


def deterministic_sync_state_id(source_name: str, scope_key: str) -> str:
    return f"sync-state:{source_name}:{scope_key}"


def repo_scope_key(repo: str = DEFAULT_SYNC_REPO) -> str:
    return f"repo:{repo}"


def github_scope_key(stream_name: str, *, repo: str = DEFAULT_SYNC_REPO) -> str:
    return f"repo:{repo}:{stream_name}"


def project_scope_key(
    project_owner: str = DEFAULT_PROJECT_OWNER,
    project_number: int = DEFAULT_PROJECT_NUMBER,
) -> str:
    return f"project:{project_owner}/{project_number}"


def github_stream_updated_since(record: SyncStateRecord | None) -> datetime | None:
    if record is None:
        return None
    return record.watermark_timestamp


def project_cursor(record: SyncStateRecord | None) -> str | None:
    if record is None:
        return None
    return record.cursor_text


def build_sync_attempt(
    source_name: str,
    scope_key: str,
    *,
    attempted_at: datetime,
    metadata: dict[str, Any] | None = None,
) -> PreparedSyncAttempt:
    return PreparedSyncAttempt(
        id=deterministic_sync_state_id(source_name, scope_key),
        source_name=source_name,
        scope_key=scope_key,
        attempted_at=attempted_at,
        metadata=dict(metadata) if metadata is not None else None,
    )


def build_sync_failure(
    source_name: str,
    scope_key: str,
    *,
    attempted_at: datetime,
    error: Exception | str,
    metadata: dict[str, Any] | None = None,
) -> PreparedSyncFailure:
    return PreparedSyncFailure(
        id=deterministic_sync_state_id(source_name, scope_key),
        source_name=source_name,
        scope_key=scope_key,
        attempted_at=attempted_at,
        error=_bounded_error_message(error),
        metadata=dict(metadata) if metadata is not None else None,
    )


def build_docs_sync_state(
    result: Any,
    *,
    attempted_at: datetime,
    succeeded_at: datetime,
    repo: str = DEFAULT_SYNC_REPO,
) -> PreparedSyncState:
    return PreparedSyncState(
        id=deterministic_sync_state_id("docs", repo_scope_key(repo)),
        source_name="docs",
        scope_key=repo_scope_key(repo),
        repo_commit_hash=getattr(result, "repo_commit_hash", None),
        cursor_text=None,
        watermark_text=None,
        watermark_timestamp=None,
        schema_version=None,
        last_attempted_at=attempted_at,
        last_succeeded_at=succeeded_at,
        last_error=None,
        metadata={
            "repo": repo,
            "processed_count": getattr(result, "processed_count", 0),
            "source_paths": list(getattr(result, "source_paths", ()) or ()),
        },
    )


def build_code_sync_state(
    result: Any,
    *,
    attempted_at: datetime,
    succeeded_at: datetime,
    repo: str = DEFAULT_SYNC_REPO,
) -> PreparedSyncState:
    return PreparedSyncState(
        id=deterministic_sync_state_id("code", repo_scope_key(repo)),
        source_name="code",
        scope_key=repo_scope_key(repo),
        repo_commit_hash=getattr(result, "repo_commit_hash", None),
        cursor_text=None,
        watermark_text=None,
        watermark_timestamp=None,
        schema_version=None,
        last_attempted_at=attempted_at,
        last_succeeded_at=succeeded_at,
        last_error=None,
        metadata={
            "repo": repo,
            "processed_file_count": getattr(result, "processed_file_count", 0),
            "chunk_count": getattr(result, "chunk_count", 0),
            "source_paths": list(getattr(result, "source_paths", ()) or ()),
        },
    )


def build_github_sync_state_updates(
    result: Any,
    *,
    attempted_at: datetime,
    succeeded_at: datetime,
) -> list[PreparedSyncState]:
    repo = getattr(result, "repo", DEFAULT_SYNC_REPO)
    bounds = getattr(result, "bounds", None)
    states: list[PreparedSyncState] = []

    for stream_name, progress in sorted(
        getattr(result, "stream_progress", {}).items(), key=lambda item: item[0]
    ):
        watermark_timestamp = getattr(progress, "latest_source_updated_at", None)
        states.append(
            PreparedSyncState(
                id=deterministic_sync_state_id("github", github_scope_key(stream_name, repo=repo)),
                source_name="github",
                scope_key=github_scope_key(stream_name, repo=repo),
                repo_commit_hash=None,
                cursor_text=None,
                watermark_text=_format_timestamp(watermark_timestamp),
                watermark_timestamp=watermark_timestamp,
                schema_version=None,
                last_attempted_at=attempted_at,
                last_succeeded_at=succeeded_at,
                last_error=None,
                metadata={
                    "repo": repo,
                    "stream_name": stream_name,
                    "page_size": getattr(bounds, "page_size", None),
                    "max_pages": _max_pages_for_stream(bounds, stream_name),
                    "updated_since": _format_timestamp(getattr(bounds, "updated_since", None)),
                    "pages_fetched": getattr(progress, "pages_fetched", 0),
                    "hit_bound": getattr(progress, "hit_bound", False),
                    "issues_written": getattr(progress, "issues_written", 0),
                    "issue_comments_written": getattr(progress, "issue_comments_written", 0),
                    "prs_written": getattr(progress, "prs_written", 0),
                    "pr_comments_written": getattr(progress, "pr_comments_written", 0),
                },
            )
        )

    return states


def build_project_sync_state(
    result: Any,
    *,
    attempted_at: datetime,
    succeeded_at: datetime,
) -> PreparedSyncState:
    owner = getattr(result, "project_owner", DEFAULT_PROJECT_OWNER)
    number = getattr(result, "project_number", DEFAULT_PROJECT_NUMBER)
    bounds = getattr(result, "bounds", None)
    watermark_timestamp = getattr(result, "latest_source_updated_at", None)
    return PreparedSyncState(
        id=deterministic_sync_state_id("project", project_scope_key(owner, number)),
        source_name="project",
        scope_key=project_scope_key(owner, number),
        repo_commit_hash=None,
        cursor_text=getattr(result, "final_cursor", None),
        watermark_text=_format_timestamp(watermark_timestamp),
        watermark_timestamp=watermark_timestamp,
        schema_version=None,
        last_attempted_at=attempted_at,
        last_succeeded_at=succeeded_at,
        last_error=None,
        metadata={
            "project_owner": owner,
            "project_number": number,
            "project_title": getattr(result, "project_title", None),
            "project_url": getattr(result, "project_url", None),
            "page_size": getattr(bounds, "page_size", None),
            "max_pages": getattr(bounds, "max_pages", None),
            "after_cursor": getattr(bounds, "after_cursor", None),
            "pages_fetched": getattr(result, "pages_fetched", 0),
            "hit_bound": getattr(result, "hit_bound", False),
            "rows_written": getattr(result, "rows_written", 0),
        },
    )


def persist_sync_state_updates(
    sync_store: SyncStateStore,
    states: Sequence[PreparedSyncState],
) -> int:
    return sync_store.upsert_sync_states(states)


def get_sync_state(
    sync_store: SyncStateStore,
    source_name: str,
    scope_key: str,
) -> SyncStateRecord | None:
    return sync_store.get_sync_state(source_name, scope_key)


def list_sync_states(
    sync_store: SyncStateStore,
    *,
    source_name: str | None = None,
) -> list[SyncStateRecord]:
    return sync_store.list_sync_states(source_name=source_name)


class PostgresSyncStateStore:
    def __init__(self, connection: Any):
        self._connection = connection

    @classmethod
    def from_database_url(cls, database_url: str) -> "PostgresSyncStateStore":
        psycopg, _ = _load_psycopg()
        return cls(psycopg.connect(database_url))

    def __enter__(self) -> "PostgresSyncStateStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def record_attempts(self, attempts: Sequence[PreparedSyncAttempt]) -> int:
        if not attempts:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_sync_state (
                        id,
                        source_name,
                        scope_key,
                        last_attempted_at,
                        metadata,
                        updated_at
                    ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        NOW()
                    )
                    ON CONFLICT (source_name, scope_key) DO UPDATE SET
                        id = EXCLUDED.id,
                        last_attempted_at = EXCLUDED.last_attempted_at,
                        metadata = COALESCE(EXCLUDED.metadata, agentic.kb_sync_state.metadata),
                        updated_at = NOW()
                    """,
                    [
                        (
                            attempt.id,
                            attempt.source_name,
                            attempt.scope_key,
                            attempt.attempted_at,
                            Json(attempt.metadata) if attempt.metadata is not None else None,
                        )
                        for attempt in attempts
                    ],
                )
        return len(attempts)

    def record_failures(self, failures: Sequence[PreparedSyncFailure]) -> int:
        if not failures:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_sync_state (
                        id,
                        source_name,
                        scope_key,
                        last_attempted_at,
                        last_error,
                        metadata,
                        updated_at
                    ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        NOW()
                    )
                    ON CONFLICT (source_name, scope_key) DO UPDATE SET
                        id = EXCLUDED.id,
                        last_attempted_at = EXCLUDED.last_attempted_at,
                        last_error = EXCLUDED.last_error,
                        metadata = COALESCE(EXCLUDED.metadata, agentic.kb_sync_state.metadata),
                        updated_at = NOW()
                    """,
                    [
                        (
                            failure.id,
                            failure.source_name,
                            failure.scope_key,
                            failure.attempted_at,
                            failure.error,
                            Json(failure.metadata) if failure.metadata is not None else None,
                        )
                        for failure in failures
                    ],
                )
        return len(failures)

    def upsert_sync_states(self, states: Sequence[PreparedSyncState]) -> int:
        if not states:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_sync_state (
                        id,
                        source_name,
                        scope_key,
                        repo_commit_hash,
                        cursor_text,
                        watermark_text,
                        watermark_timestamp,
                        schema_version,
                        last_attempted_at,
                        last_succeeded_at,
                        last_error,
                        metadata,
                        updated_at
                    ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        NOW()
                    )
                    ON CONFLICT (source_name, scope_key) DO UPDATE SET
                        id = EXCLUDED.id,
                        repo_commit_hash = EXCLUDED.repo_commit_hash,
                        cursor_text = EXCLUDED.cursor_text,
                        watermark_text = EXCLUDED.watermark_text,
                        watermark_timestamp = EXCLUDED.watermark_timestamp,
                        schema_version = EXCLUDED.schema_version,
                        last_attempted_at = EXCLUDED.last_attempted_at,
                        last_succeeded_at = EXCLUDED.last_succeeded_at,
                        last_error = EXCLUDED.last_error,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            state.id,
                            state.source_name,
                            state.scope_key,
                            state.repo_commit_hash,
                            state.cursor_text,
                            state.watermark_text,
                            state.watermark_timestamp,
                            state.schema_version,
                            state.last_attempted_at,
                            state.last_succeeded_at,
                            state.last_error,
                            Json(state.metadata),
                        )
                        for state in states
                    ],
                )
        return len(states)

    def get_sync_state(self, source_name: str, scope_key: str) -> SyncStateRecord | None:
        with self._connection.cursor() as cursor:
            cursor.execute(
                """
                SELECT
                    id,
                    source_name,
                    scope_key,
                    repo_commit_hash,
                    cursor_text,
                    watermark_text,
                    watermark_timestamp,
                    schema_version,
                    last_attempted_at,
                    last_succeeded_at,
                    last_error,
                    metadata,
                    created_at,
                    updated_at
                FROM agentic.kb_sync_state
                WHERE source_name = %s AND scope_key = %s
                """,
                (source_name, scope_key),
            )
            row = cursor.fetchone()
        if row is None:
            return None
        return _record_from_row(row)

    def list_sync_states(self, *, source_name: str | None = None) -> list[SyncStateRecord]:
        query = """
            SELECT
                id,
                source_name,
                scope_key,
                repo_commit_hash,
                cursor_text,
                watermark_text,
                watermark_timestamp,
                schema_version,
                last_attempted_at,
                last_succeeded_at,
                last_error,
                metadata,
                created_at,
                updated_at
            FROM agentic.kb_sync_state
        """
        params: tuple[Any, ...] = ()
        if source_name is not None:
            query += " WHERE source_name = %s"
            params = (source_name,)
        query += " ORDER BY source_name ASC, scope_key ASC"
        with self._connection.cursor() as cursor:
            cursor.execute(query, params)
            rows = cursor.fetchall()
        return [_record_from_row(row) for row in rows]


class InMemorySyncStateStore:
    def __init__(self):
        self.rows_by_key: dict[tuple[str, str], dict[str, Any]] = {}
        self.write_count = 0

    def record_attempts(self, attempts: Sequence[PreparedSyncAttempt]) -> int:
        for attempt in attempts:
            row = self._ensure_row(attempt.source_name, attempt.scope_key, attempt.id)
            row["last_attempted_at"] = attempt.attempted_at
            if attempt.metadata is not None:
                row["metadata"] = dict(attempt.metadata)
            row["updated_at_token"] = self._next_token()
        return len(attempts)

    def record_failures(self, failures: Sequence[PreparedSyncFailure]) -> int:
        for failure in failures:
            row = self._ensure_row(failure.source_name, failure.scope_key, failure.id)
            row["last_attempted_at"] = failure.attempted_at
            row["last_error"] = failure.error
            if failure.metadata is not None:
                row["metadata"] = dict(failure.metadata)
            row["updated_at_token"] = self._next_token()
        return len(failures)

    def upsert_sync_states(self, states: Sequence[PreparedSyncState]) -> int:
        for state in states:
            row = self._ensure_row(state.source_name, state.scope_key, state.id)
            row.update(
                {
                    "id": state.id,
                    "repo_commit_hash": state.repo_commit_hash,
                    "cursor_text": state.cursor_text,
                    "watermark_text": state.watermark_text,
                    "watermark_timestamp": state.watermark_timestamp,
                    "schema_version": state.schema_version,
                    "last_attempted_at": state.last_attempted_at,
                    "last_succeeded_at": state.last_succeeded_at,
                    "last_error": state.last_error,
                    "metadata": dict(state.metadata),
                    "updated_at_token": self._next_token(),
                }
            )
        return len(states)

    def get_sync_state(self, source_name: str, scope_key: str) -> SyncStateRecord | None:
        row = self.rows_by_key.get((source_name, scope_key))
        if row is None:
            return None
        return _record_from_mapping(row)

    def list_sync_states(self, *, source_name: str | None = None) -> list[SyncStateRecord]:
        rows = []
        for (row_source_name, _), row in sorted(self.rows_by_key.items()):
            if source_name is not None and row_source_name != source_name:
                continue
            rows.append(_record_from_mapping(row))
        return rows

    def _ensure_row(self, source_name: str, scope_key: str, row_id: str) -> dict[str, Any]:
        key = (source_name, scope_key)
        if key not in self.rows_by_key:
            now = _utcnow()
            self.rows_by_key[key] = {
                "id": row_id,
                "source_name": source_name,
                "scope_key": scope_key,
                "repo_commit_hash": None,
                "cursor_text": None,
                "watermark_text": None,
                "watermark_timestamp": None,
                "schema_version": None,
                "last_attempted_at": None,
                "last_succeeded_at": None,
                "last_error": None,
                "metadata": {},
                "created_at": now,
                "updated_at": now,
                "updated_at_token": 0,
            }
        return self.rows_by_key[key]

    def _next_token(self) -> int:
        self.write_count += 1
        return self.write_count


def _record_from_row(row: Sequence[Any]) -> SyncStateRecord:
    return SyncStateRecord(
        id=row[0],
        source_name=row[1],
        scope_key=row[2],
        repo_commit_hash=row[3],
        cursor_text=row[4],
        watermark_text=row[5],
        watermark_timestamp=row[6],
        schema_version=row[7],
        last_attempted_at=row[8],
        last_succeeded_at=row[9],
        last_error=row[10],
        metadata=dict(row[11] or {}),
        created_at=row[12],
        updated_at=row[13],
    )


def _record_from_mapping(row: dict[str, Any]) -> SyncStateRecord:
    updated_at = row.get("updated_at") or _utcnow()
    return SyncStateRecord(
        id=row["id"],
        source_name=row["source_name"],
        scope_key=row["scope_key"],
        repo_commit_hash=row.get("repo_commit_hash"),
        cursor_text=row.get("cursor_text"),
        watermark_text=row.get("watermark_text"),
        watermark_timestamp=row.get("watermark_timestamp"),
        schema_version=row.get("schema_version"),
        last_attempted_at=row.get("last_attempted_at"),
        last_succeeded_at=row.get("last_succeeded_at"),
        last_error=row.get("last_error"),
        metadata=dict(row.get("metadata") or {}),
        created_at=row.get("created_at") or updated_at,
        updated_at=updated_at,
    )


def _bounded_error_message(error: Exception | str) -> str:
    message = str(error).strip() or error.__class__.__name__
    if len(message) <= MAX_SYNC_ERROR_LENGTH:
        return message
    return message[: MAX_SYNC_ERROR_LENGTH - 3].rstrip() + "..."


def _format_timestamp(value: datetime | None) -> str | None:
    if value is None:
        return None
    return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def _max_pages_for_stream(bounds: Any, stream_name: str) -> int | None:
    if bounds is None:
        return None
    page_limit_for_stream = getattr(bounds, "page_limit_for_stream", None)
    if callable(page_limit_for_stream):
        return page_limit_for_stream(stream_name)
    return getattr(bounds, "max_pages", None)


def _utcnow() -> datetime:
    return datetime.now(timezone.utc)


def _load_psycopg() -> tuple[Any, Any]:
    import psycopg
    from psycopg.types.json import Json

    return psycopg, Json
