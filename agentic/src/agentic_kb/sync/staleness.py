from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from subprocess import CalledProcessError, run
from typing import Callable

from agentic_kb.ingest.github import (
    DEFAULT_REQUEST_TIMEOUT_SECONDS as DEFAULT_GITHUB_REQUEST_TIMEOUT_SECONDS,
    fetch_latest_github_stream_watermarks,
)
from agentic_kb.ingest.project import (
    DEFAULT_REQUEST_TIMEOUT_SECONDS as DEFAULT_PROJECT_REQUEST_TIMEOUT_SECONDS,
    fetch_latest_project_watermark,
)
from agentic_kb.sync.state import (
    DEFAULT_PROJECT_NUMBER,
    DEFAULT_PROJECT_OWNER,
    DEFAULT_SYNC_REPO,
    GITHUB_STREAM_NAMES,
    SyncStateRecord,
    SyncStateStore,
    github_scope_key,
    project_scope_key,
    repo_scope_key,
)


@dataclass(frozen=True)
class FreshnessItem:
    name: str
    status: str
    detail: str
    baseline: str | None = None
    observed: str | None = None


@dataclass(frozen=True)
class FreshnessReport:
    up_to_date: bool
    items: tuple[FreshnessItem, ...]


def collect_freshness_report(
    *,
    workspace_root: str | Path,
    sync_store: SyncStateStore,
    github_token: str | None,
    head_commit_getter: Callable[[Path], str] | None = None,
    github_watermarks_fetcher: Callable[..., dict[str, datetime | None]] = fetch_latest_github_stream_watermarks,
    project_watermark_fetcher: Callable[..., datetime | None] = fetch_latest_project_watermark,
    github_request_timeout_seconds: int = DEFAULT_GITHUB_REQUEST_TIMEOUT_SECONDS,
    project_request_timeout_seconds: int = DEFAULT_PROJECT_REQUEST_TIMEOUT_SECONDS,
) -> FreshnessReport:
    root = Path(workspace_root).resolve()
    items: list[FreshnessItem] = []

    head_error: str | None = None
    head_commit: str | None = None
    docs_record = sync_store.get_sync_state("docs", repo_scope_key(DEFAULT_SYNC_REPO))
    code_record = sync_store.get_sync_state("code", repo_scope_key(DEFAULT_SYNC_REPO))

    if _has_successful_repo_baseline(docs_record) or _has_successful_repo_baseline(code_record):
        try:
            head_commit = (head_commit_getter or get_head_commit)(root)
        except Exception as error:  # pragma: no cover - covered through caller-facing tests
            head_error = str(error)

    items.append(
        _build_local_freshness_item(
            "docs",
            docs_record,
            head_commit=head_commit,
            head_error=head_error,
        )
    )
    items.append(
        _build_local_freshness_item(
            "code",
            code_record,
            head_commit=head_commit,
            head_error=head_error,
        )
    )

    if not github_token:
        skip_detail = "skipped because GITHUB_TOKEN is not configured"
        items.extend(
            FreshnessItem(name=f"github {stream_name}", status="skipped", detail=skip_detail)
            for stream_name in GITHUB_STREAM_NAMES
        )
        items.append(FreshnessItem(name="project", status="skipped", detail=skip_detail))
    else:
        github_remote: dict[str, datetime | None] | None = None
        github_error: str | None = None
        try:
            github_remote = github_watermarks_fetcher(
                repo=DEFAULT_SYNC_REPO,
                github_token=github_token,
                request_timeout_seconds=github_request_timeout_seconds,
            )
        except Exception as error:
            github_error = str(error)

        for stream_name in GITHUB_STREAM_NAMES:
            items.append(
                _build_github_freshness_item(
                    stream_name,
                    sync_store.get_sync_state(
                        "github",
                        github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
                    ),
                    remote_watermarks=github_remote,
                    remote_error=github_error,
                )
            )

        project_error: str | None = None
        project_remote: datetime | None = None
        try:
            project_remote = project_watermark_fetcher(
                github_token=github_token,
                project_owner=DEFAULT_PROJECT_OWNER,
                project_number=DEFAULT_PROJECT_NUMBER,
                request_timeout_seconds=project_request_timeout_seconds,
            )
        except Exception as error:
            project_error = str(error)

        items.append(
            _build_project_freshness_item(
                sync_store.get_sync_state(
                    "project",
                    project_scope_key(
                        DEFAULT_PROJECT_OWNER,
                        DEFAULT_PROJECT_NUMBER,
                    ),
                ),
                remote_watermark=project_remote,
                remote_error=project_error,
            )
        )

    return FreshnessReport(
        up_to_date=all(item.status == "fresh" for item in items),
        items=tuple(items),
    )


def get_head_commit(workspace_root: Path) -> str:
    try:
        completed = run(
            ["git", "-c", f"safe.directory={workspace_root}", "rev-parse", "HEAD"],
            check=True,
            capture_output=True,
            text=True,
            cwd=workspace_root,
        )
    except FileNotFoundError as error:
        raise RuntimeError("git is required for stale-index detection") from error
    except CalledProcessError as error:
        stderr = (error.stderr or "").strip()
        stdout = (error.stdout or "").strip()
        detail = stderr or stdout or str(error)
        raise RuntimeError(detail) from error
    return completed.stdout.strip()


def serialize_freshness_report(report: FreshnessReport) -> dict[str, object]:
    return {
        "up_to_date": report.up_to_date,
        "items": [serialize_freshness_item(item) for item in report.items],
    }


def serialize_freshness_item(item: FreshnessItem) -> dict[str, object]:
    return {
        "name": item.name,
        "status": item.status,
        "detail": item.detail,
        "baseline": item.baseline,
        "observed": item.observed,
    }


def _build_local_freshness_item(
    source_name: str,
    record: SyncStateRecord | None,
    *,
    head_commit: str | None,
    head_error: str | None,
) -> FreshnessItem:
    if not _has_successful_repo_baseline(record):
        return FreshnessItem(
            name=source_name,
            status="missing_baseline",
            detail="missing successful baseline with repo_commit_hash",
        )
    if head_error is not None or head_commit is None:
        return FreshnessItem(
            name=source_name,
            status="unavailable",
            detail=f"unable to inspect local HEAD: {head_error or 'unknown error'}",
            baseline=record.repo_commit_hash,
        )
    if record.repo_commit_hash == head_commit:
        return FreshnessItem(
            name=source_name,
            status="fresh",
            detail="baseline commit matches local HEAD",
            baseline=record.repo_commit_hash,
            observed=head_commit,
        )
    return FreshnessItem(
        name=source_name,
        status="stale",
        detail="baseline commit differs from local HEAD",
        baseline=record.repo_commit_hash,
        observed=head_commit,
    )


def _build_github_freshness_item(
    stream_name: str,
    record: SyncStateRecord | None,
    *,
    remote_watermarks: dict[str, datetime | None] | None,
    remote_error: str | None,
) -> FreshnessItem:
    if not _has_successful_watermark(record):
        return FreshnessItem(
            name=f"github {stream_name}",
            status="missing_baseline",
            detail="missing successful baseline with watermark_timestamp",
        )
    if remote_error is not None or remote_watermarks is None:
        return FreshnessItem(
            name=f"github {stream_name}",
            status="unavailable",
            detail=f"unable to inspect latest GitHub watermark: {remote_error or 'unknown error'}",
            baseline=_format_value(record.watermark_timestamp),
        )
    remote_watermark = remote_watermarks.get(stream_name)
    if remote_watermark is None:
        return FreshnessItem(
            name=f"github {stream_name}",
            status="fresh",
            detail="no remote items observed for this stream",
            baseline=_format_value(record.watermark_timestamp),
        )
    if record.watermark_timestamp >= remote_watermark:
        return FreshnessItem(
            name=f"github {stream_name}",
            status="fresh",
            detail="stored watermark matches or exceeds the latest GitHub update",
            baseline=_format_value(record.watermark_timestamp),
            observed=_format_value(remote_watermark),
        )
    return FreshnessItem(
        name=f"github {stream_name}",
        status="stale",
        detail="stored watermark lags behind the latest GitHub update",
        baseline=_format_value(record.watermark_timestamp),
        observed=_format_value(remote_watermark),
    )


def _build_project_freshness_item(
    record: SyncStateRecord | None,
    *,
    remote_watermark: datetime | None,
    remote_error: str | None,
) -> FreshnessItem:
    if not _has_successful_watermark(record):
        return FreshnessItem(
            name="project",
            status="missing_baseline",
            detail="missing successful baseline with watermark_timestamp",
        )
    if remote_error is not None:
        return FreshnessItem(
            name="project",
            status="unavailable",
            detail=f"unable to inspect latest project watermark: {remote_error}",
            baseline=_format_value(record.watermark_timestamp),
        )
    if remote_watermark is None:
        return FreshnessItem(
            name="project",
            status="fresh",
            detail="no remote project items observed",
            baseline=_format_value(record.watermark_timestamp),
        )
    if record.watermark_timestamp >= remote_watermark:
        return FreshnessItem(
            name="project",
            status="fresh",
            detail="stored project watermark matches or exceeds the latest observed project item",
            baseline=_format_value(record.watermark_timestamp),
            observed=_format_value(remote_watermark),
        )
    return FreshnessItem(
        name="project",
        status="stale",
        detail=(
            "stored project watermark lags behind the latest observed project item; "
            "cursor continuation still cannot guarantee replay of older edits"
        ),
        baseline=_format_value(record.watermark_timestamp),
        observed=_format_value(remote_watermark),
    )


def _has_successful_repo_baseline(record: SyncStateRecord | None) -> bool:
    return bool(record is not None and record.last_succeeded_at is not None and record.repo_commit_hash)


def _has_successful_watermark(record: SyncStateRecord | None) -> bool:
    return bool(record is not None and record.last_succeeded_at is not None and record.watermark_timestamp is not None)


def _format_value(value: datetime | str | None) -> str | None:
    if value is None:
        return None
    if isinstance(value, datetime):
        return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
    return value
