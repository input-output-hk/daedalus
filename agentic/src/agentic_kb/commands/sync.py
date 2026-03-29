from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from subprocess import CalledProcessError, run
from typing import Any, Sequence

from agentic_kb.commands.output import print_stderr
from agentic_kb.config import AgenticConfig
from agentic_kb.embed import OllamaEmbeddingClient
from agentic_kb.ingest.docs import (
    PostgresDocsStore,
    is_allowlisted_doc_path,
)
from agentic_kb.ingest.github import (
    GithubApiClient,
    GithubFetchBounds,
    PostgresGithubStore,
    GithubIngestResult,
    _GithubParentCache,
    _MutableStreamProgress,
    _prepare_page_batch,
    write_github_page_batch,
)
from agentic_kb.ingest.project import (
    PostgresProjectItemsStore,
    ProjectFetchBounds,
    ingest_project_items,
)
from agentic_kb.sync.state import (
    DEFAULT_PROJECT_NUMBER,
    DEFAULT_PROJECT_OWNER,
    DEFAULT_SYNC_REPO,
    GITHUB_STREAM_NAMES,
    PostgresSyncStateStore,
    PreparedSyncState,
    SyncStateRecord,
    build_code_sync_state,
    build_docs_sync_state,
    build_github_sync_state_updates,
    build_project_sync_state,
    build_sync_attempt,
    build_sync_failure,
    github_scope_key,
    github_stream_updated_since,
    project_cursor,
    project_scope_key,
    repo_scope_key,
)

try:
    from agentic_kb.ingest.code import PostgresCodeChunksStore, ingest_code, is_supported_code_path
except ModuleNotFoundError as error:
    if error.name not in {"tree_sitter", "tree_sitter_language_pack"}:
        raise
    PostgresCodeChunksStore = None
    ingest_code = None
    is_supported_code_path = None


SYNC_TASK_GUIDANCE = {
    "all": "task-701 will implement the real sync orchestration after schema and ingestion foundations land.",
    "changed": "task-604 implements only the post-import bootstrap flow backed by restored sync state.",
    "docs": "task-701 will implement docs sync on top of the docs ingestion work from task-301.",
    "code": "task-701 will implement code sync on top of the code ingestion work from task-401 and task-402.",
    "github": "task-701 will implement GitHub sync after task-403 is available.",
    "project": "task-701 will implement project sync after task-404 is available.",
}
SUPPORTED_GITHUB_BOOTSTRAP_STREAMS = ("issues", "issue_comments")
DEFERRED_GITHUB_BOOTSTRAP_STREAMS = ("pulls", "review_comments")


class SyncCommandError(RuntimeError):
    pass


@dataclass(frozen=True)
class LocalDelta:
    changed_paths: tuple[str, ...]
    deleted_paths: tuple[str, ...]
    baseline_commit: str
    head_commit: str


@dataclass(frozen=True)
class RepoDeltaEntry:
    status: str
    old_path: str | None
    new_path: str | None


def add_sync_subcommands(parser) -> None:
    subparsers = parser.add_subparsers(dest="sync_command", required=True)

    for name in ("all", "docs", "code", "github", "project"):
        command_parser = subparsers.add_parser(name, help=f"Placeholder for sync {name}")
        command_parser.set_defaults(handler=_run_sync_placeholder, placeholder_name=name)

    changed_parser = subparsers.add_parser(
        "changed",
        help="Sync only local changes and bounded remote continuation after snapshot import",
    )
    changed_parser.set_defaults(handler=run_sync_changed)


def run_sync_changed(args) -> int:
    try:
        config = AgenticConfig.from_env()
        workspace_root = Path.cwd()
        result = sync_changed(workspace_root, config=config)
    except SyncCommandError as error:
        print_stderr(f"sync changed failed: {error}")
        return 1
    except Exception as error:
        print_stderr(f"sync changed failed: {error}")
        return 1

    for line in format_sync_changed_output(result):
        print(line)
    return 0


def sync_changed(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync changed")
    if PostgresCodeChunksStore is None or ingest_code is None or is_supported_code_path is None:
        raise SyncCommandError(
            "sync changed requires the optional tree-sitter code-ingest dependencies to be installed"
        )

    root = Path(workspace_root).resolve()
    attempted_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)

    with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
        baselines = load_required_baselines(sync_store)
        docs_delta = compute_docs_delta(root, baselines["docs"].repo_commit_hash or "")
        code_delta = compute_code_delta(root, baselines["code"].repo_commit_hash or "")

        with PostgresDocsStore.from_database_url(config.database_url) as docs_store:
            docs_result = _sync_docs_changed(
                root,
                embedding_client=embedding_client,
                docs_store=docs_store,
                sync_store=sync_store,
                baseline=baselines["docs"],
                delta=docs_delta,
                attempted_at=attempted_at,
            )

        with PostgresCodeChunksStore.from_database_url(config.database_url) as code_store:
            code_result = _sync_code_changed(
                root,
                embedding_client=embedding_client,
                code_store=code_store,
                sync_store=sync_store,
                baseline=baselines["code"],
                delta=code_delta,
                attempted_at=attempted_at,
            )

        with PostgresGithubStore.from_database_url(config.database_url) as github_store:
            github_result = _sync_github_changed(
                config=config,
                embedding_client=embedding_client,
                github_store=github_store,
                sync_store=sync_store,
                baselines=baselines,
                attempted_at=attempted_at,
            )

        with PostgresProjectItemsStore.from_database_url(config.database_url) as project_store:
            project_result = _sync_project_changed(
                config=config,
                embedding_client=embedding_client,
                project_store=project_store,
                sync_store=sync_store,
                baseline=baselines["project"],
                attempted_at=attempted_at,
            )

    return {
        "docs": docs_result,
        "code": code_result,
        "github": github_result,
        "project": project_result,
        "head_commit": get_head_commit(root),
    }


def format_sync_changed_output(result: dict[str, Any]) -> list[str]:
    docs_result = result["docs"]
    code_result = result["code"]
    github_result = result["github"]
    project_result = result["project"]
    lines = [
        "sync changed completed for the post-import bootstrap path",
        (
            "docs: "
            f"updated={len(docs_result['changed_paths'])}, "
            f"deleted={len(docs_result['deleted_paths'])}, "
            f"baseline={docs_result['baseline_commit']} -> {docs_result['repo_commit_hash']}"
        ),
        (
            "code: "
            f"updated={len(code_result['changed_paths'])}, "
            f"deleted={len(code_result['deleted_paths'])}, "
            f"baseline={code_result['baseline_commit']} -> {code_result['repo_commit_hash']}"
        ),
        (
            "github: "
            f"bounded streams synced={', '.join(github_result['synced_streams'])}; "
            f"deferred streams={', '.join(github_result['deferred_streams'])}"
        ),
        (
            "project: cursor continuation synced "
            f"from {project_result['starting_cursor'] or '<start>'} "
            f"to {project_result['final_cursor'] or '<unchanged>'}"
        ),
        "note: GitHub bounded guarantees in task-604 apply only to issues and issue_comments; pulls and review_comments remain deferred.",
        "note: Project task-604 behavior is cursor continuation only and does not detect updates to already-seen items.",
    ]
    return lines


def load_required_baselines(sync_store: PostgresSyncStateStore) -> dict[str, Any]:
    required: dict[str, SyncStateRecord | dict[str, SyncStateRecord]] = {
        "docs": require_sync_state(sync_store, "docs", repo_scope_key(DEFAULT_SYNC_REPO)),
        "code": require_sync_state(sync_store, "code", repo_scope_key(DEFAULT_SYNC_REPO)),
        "project": require_sync_state(
            sync_store,
            "project",
            project_scope_key(DEFAULT_PROJECT_OWNER, DEFAULT_PROJECT_NUMBER),
        ),
        "github": {
            stream_name: require_sync_state(
                sync_store,
                "github",
                github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
            )
            for stream_name in GITHUB_STREAM_NAMES
        },
    }

    for source_name in ("docs", "code"):
        record = required[source_name]
        if not isinstance(record, SyncStateRecord) or not record.repo_commit_hash:
            raise SyncCommandError(
                f"sync changed requires a restored {source_name} baseline repo commit; import a validated snapshot first"
            )

    project_baseline = required["project"]
    if not isinstance(project_baseline, SyncStateRecord):
        raise SyncCommandError("sync changed requires a restored Project baseline")
    require_project_cursor_baseline(project_baseline)

    github_baselines = required["github"]
    if not isinstance(github_baselines, dict):
        raise SyncCommandError("sync changed requires restored GitHub stream baselines")
    for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS:
        watermark = github_stream_updated_since(github_baselines[stream_name])
        if watermark is None:
            raise SyncCommandError(
                "sync changed requires restored GitHub updated_since watermarks for bounded bootstrap refresh; "
                f"missing github:{github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO)} watermark. "
                "Import a validated snapshot first instead of widening to an unbounded replay."
            )

    return required


def require_sync_state(
    sync_store: PostgresSyncStateStore,
    source_name: str,
    scope_key: str,
) -> SyncStateRecord:
    record = sync_store.get_sync_state(source_name, scope_key)
    if record is None:
        raise SyncCommandError(
            "sync changed requires imported sync-state baselines for docs, code, github streams, and project; "
            f"missing {source_name}:{scope_key}. Import a validated snapshot first."
        )
    return record


def require_project_cursor_baseline(record: SyncStateRecord) -> str:
    cursor = project_cursor(record)
    if cursor is None or not cursor.strip():
        raise SyncCommandError(
            "sync changed requires a restored non-empty Project cursor baseline for cursor continuation; "
            f"missing project:{record.scope_key} cursor_text. "
            "Import a validated snapshot first instead of widening to after_cursor=None."
        )
    return cursor


def compute_docs_delta(workspace_root: Path, baseline_commit: str) -> LocalDelta:
    return compute_repo_delta(
        workspace_root,
        baseline_commit=baseline_commit,
        path_filter=is_allowlisted_doc_path,
    )


def compute_code_delta(workspace_root: Path, baseline_commit: str) -> LocalDelta:
    return compute_repo_delta(
        workspace_root,
        baseline_commit=baseline_commit,
        path_filter=is_supported_code_path,
    )


def compute_repo_delta(
    workspace_root: Path,
    *,
    baseline_commit: str,
    path_filter,
) -> LocalDelta:
    head_commit = get_head_commit(workspace_root)
    delta_entries = list_repo_delta_entries(workspace_root, baseline_commit)

    changed_paths: list[str] = []
    deleted_paths: list[str] = []
    for entry in delta_entries:
        if entry.status in {"A", "C", "M"} and entry.new_path is not None and path_filter(entry.new_path):
            changed_paths.append(entry.new_path)
        elif entry.status == "D" and entry.old_path is not None and path_filter(entry.old_path):
            deleted_paths.append(entry.old_path)
        elif entry.status == "R":
            if entry.old_path is not None and path_filter(entry.old_path):
                deleted_paths.append(entry.old_path)
            if entry.new_path is not None and path_filter(entry.new_path):
                changed_paths.append(entry.new_path)

    return LocalDelta(
        changed_paths=tuple(_unique_paths(changed_paths)),
        deleted_paths=tuple(_unique_paths(deleted_paths)),
        baseline_commit=baseline_commit,
        head_commit=head_commit,
    )


def get_head_commit(workspace_root: Path) -> str:
    return _run_git_command(workspace_root, ["rev-parse", "HEAD"])


def list_repo_delta_entries(workspace_root: Path, baseline_commit: str) -> list[RepoDeltaEntry]:
    _ensure_commit_exists(workspace_root, baseline_commit)
    output = _run_git_command(
        workspace_root,
        ["diff", "--name-status", "--find-renames", "--diff-filter=ACMRD", baseline_commit, "HEAD"],
    )
    if not output:
        return []
    entries: list[RepoDeltaEntry] = []
    for raw_line in output.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        parts = line.split("\t")
        status = parts[0][:1]
        if status in {"A", "M", "D"} and len(parts) >= 2:
            path = parts[1].strip()
            entries.append(
                RepoDeltaEntry(
                    status=status,
                    old_path=path if status == "D" else None,
                    new_path=path if status != "D" else None,
                )
            )
            continue
        if status == "C" and len(parts) >= 3:
            entries.append(
                RepoDeltaEntry(
                    status=status,
                    old_path=parts[1].strip(),
                    new_path=parts[2].strip(),
                )
            )
            continue
        if status == "R" and len(parts) >= 3:
            entries.append(
                RepoDeltaEntry(
                    status=status,
                    old_path=parts[1].strip(),
                    new_path=parts[2].strip(),
                )
            )
            continue
        raise SyncCommandError(f"unsupported git diff name-status output: {line}")
    return entries


def _unique_paths(paths: Sequence[str]) -> list[str]:
    return list(dict.fromkeys(paths))


def _ensure_commit_exists(workspace_root: Path, commit_hash: str) -> None:
    try:
        _run_git_command(workspace_root, ["cat-file", "-e", f"{commit_hash}^{{commit}}"])
    except SyncCommandError as error:
        raise SyncCommandError(
            f"baseline commit {commit_hash} is not available in the local clone; fetch full history or import a newer snapshot"
        ) from error


def _run_git_command(workspace_root: Path, args: Sequence[str]) -> str:
    try:
        completed = run(
            ["git", *args],
            check=True,
            capture_output=True,
            text=True,
            cwd=workspace_root,
        )
    except FileNotFoundError as error:
        raise SyncCommandError("git is required for sync changed") from error
    except CalledProcessError as error:
        stderr = (error.stderr or "").strip()
        stdout = (error.stdout or "").strip()
        detail = stderr or stdout or str(error)
        raise SyncCommandError(detail) from error
    return completed.stdout.strip()


def _sync_docs_changed(
    workspace_root: Path,
    *,
    embedding_client,
    docs_store,
    sync_store: PostgresSyncStateStore,
    baseline: SyncStateRecord,
    delta: LocalDelta,
    attempted_at: datetime,
) -> dict[str, Any]:
    scope_key = baseline.scope_key
    sync_store.record_attempts(
        [build_sync_attempt("docs", scope_key, attempted_at=attempted_at, metadata={"repo": DEFAULT_SYNC_REPO})]
    )

    try:
        if delta.deleted_paths:
            docs_store.delete_documents_for_paths(delta.deleted_paths)
        if delta.changed_paths:
            result = _ingest_selected_docs(
                workspace_root,
                embedding_client=embedding_client,
                docs_store=docs_store,
                source_paths=delta.changed_paths,
                repo_commit_hash=delta.head_commit,
            )
        else:
            result = type(
                "DocsChangedResult",
                (),
                {
                    "source_paths": (),
                    "processed_count": 0,
                    "repo_commit_hash": delta.head_commit,
                },
            )()
    except Exception as error:
        sync_store.record_failures(
            [build_sync_failure("docs", scope_key, attempted_at=attempted_at, error=error, metadata={"repo": DEFAULT_SYNC_REPO})]
        )
        raise

    state = build_docs_sync_state(
        result,
        attempted_at=attempted_at,
        succeeded_at=datetime.now(timezone.utc),
        repo=DEFAULT_SYNC_REPO,
    )
    sync_store.upsert_sync_states([state])
    return {
        "changed_paths": delta.changed_paths,
        "deleted_paths": delta.deleted_paths,
        "processed_count": getattr(result, "processed_count", 0),
        "repo_commit_hash": state.repo_commit_hash,
        "baseline_commit": delta.baseline_commit,
    }


def _ingest_selected_docs(
    workspace_root: Path,
    *,
    embedding_client,
    docs_store,
    source_paths: Sequence[str],
    repo_commit_hash: str,
):
    from agentic_kb.ingest.docs import prepare_documents

    documents = prepare_documents(
        workspace_root,
        source_paths=source_paths,
        embedding_client=embedding_client,
        repo_commit_hash=repo_commit_hash,
    )
    docs_store.upsert_documents(documents)
    return type(
        "DocsChangedResult",
        (),
        {
            "source_paths": tuple(document.source_path for document in documents),
            "processed_count": len(documents),
            "repo_commit_hash": repo_commit_hash,
        },
    )()


def _sync_code_changed(
    workspace_root: Path,
    *,
    embedding_client,
    code_store,
    sync_store: PostgresSyncStateStore,
    baseline: SyncStateRecord,
    delta: LocalDelta,
    attempted_at: datetime,
) -> dict[str, Any]:
    scope_key = baseline.scope_key
    sync_store.record_attempts(
        [build_sync_attempt("code", scope_key, attempted_at=attempted_at, metadata={"repo": DEFAULT_SYNC_REPO})]
    )

    try:
        if delta.changed_paths:
            result = ingest_code(
                workspace_root,
                embedding_client=embedding_client,
                code_store=code_store,
                source_paths=delta.changed_paths,
                repo_commit_hash=delta.head_commit,
                run_mode="targeted",
                prune_missing=False,
            )
        else:
            result = type(
                "CodeChangedResult",
                (),
                {
                    "source_paths": (),
                    "processed_file_count": 0,
                    "chunk_count": 0,
                    "repo_commit_hash": delta.head_commit,
                },
            )()

        for repo_path in delta.deleted_paths:
            code_store.replace_chunks_for_path(repo_path, [])
    except Exception as error:
        sync_store.record_failures(
            [build_sync_failure("code", scope_key, attempted_at=attempted_at, error=error, metadata={"repo": DEFAULT_SYNC_REPO})]
        )
        raise

    state = build_code_sync_state(
        result,
        attempted_at=attempted_at,
        succeeded_at=datetime.now(timezone.utc),
        repo=DEFAULT_SYNC_REPO,
    )
    sync_store.upsert_sync_states([state])
    return {
        "changed_paths": delta.changed_paths,
        "deleted_paths": delta.deleted_paths,
        "processed_file_count": getattr(result, "processed_file_count", 0),
        "chunk_count": getattr(result, "chunk_count", 0),
        "repo_commit_hash": state.repo_commit_hash,
        "baseline_commit": delta.baseline_commit,
    }


def _sync_github_changed(
    *,
    config: AgenticConfig,
    embedding_client,
    github_store,
    sync_store: PostgresSyncStateStore,
    baselines: dict[str, Any],
    attempted_at: datetime,
) -> dict[str, Any]:
    if not config.github_token:
        raise SyncCommandError("GITHUB_TOKEN is required for sync changed GitHub bootstrap refresh")

    sync_store.record_attempts(
        [
            build_sync_attempt(
                "github",
                github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
                attempted_at=attempted_at,
                metadata={"repo": DEFAULT_SYNC_REPO, "stream_name": stream_name},
            )
            for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
        ]
    )

    try:
        watermarks = [
            github_stream_updated_since(baselines["github"][stream_name])
            for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
        ]
        updated_since = min(watermarks)
        github_result = _ingest_supported_github_streams(
            embedding_client=embedding_client,
            github_store=github_store,
            github_token=config.github_token,
            bounds=GithubFetchBounds(repo=DEFAULT_SYNC_REPO, updated_since=updated_since),
        )
    except Exception as error:
        sync_store.record_failures(
            [
                build_sync_failure(
                    "github",
                    github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
                    attempted_at=attempted_at,
                    error=error,
                    metadata={"repo": DEFAULT_SYNC_REPO, "stream_name": stream_name},
                )
                for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
            ]
        )
        raise

    persisted_states = [
        state
        for state in build_github_sync_state_updates(
            github_result,
            attempted_at=attempted_at,
            succeeded_at=datetime.now(timezone.utc),
        )
        if state.metadata.get("stream_name") in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
    ]

    deferred_states = [
        _build_deferred_github_state(
            baselines["github"][stream_name],
            attempted_at=attempted_at,
        )
        for stream_name in DEFERRED_GITHUB_BOOTSTRAP_STREAMS
    ]
    sync_store.upsert_sync_states([*persisted_states, *deferred_states])

    return {
        "updated_since": github_result.bounds.updated_since,
        "synced_streams": SUPPORTED_GITHUB_BOOTSTRAP_STREAMS,
        "deferred_streams": DEFERRED_GITHUB_BOOTSTRAP_STREAMS,
        "stream_progress": {
            stream_name: github_result.stream_progress[stream_name]
            for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
        },
    }


def _build_deferred_github_state(
    baseline: SyncStateRecord,
    *,
    attempted_at: datetime,
) -> PreparedSyncState:
    metadata = dict(baseline.metadata)
    metadata["task_604_status"] = "deferred"
    metadata["task_604_note"] = (
        "sync changed defers bounded bootstrap refresh for this GitHub stream until later follow-up work"
    )
    return PreparedSyncState(
        id=baseline.id,
        source_name=baseline.source_name,
        scope_key=baseline.scope_key,
        repo_commit_hash=baseline.repo_commit_hash,
        cursor_text=baseline.cursor_text,
        watermark_text=baseline.watermark_text,
        watermark_timestamp=baseline.watermark_timestamp,
        schema_version=baseline.schema_version,
        last_attempted_at=attempted_at,
        last_succeeded_at=baseline.last_succeeded_at,
        last_error=baseline.last_error,
        metadata=metadata,
    )


def _ingest_supported_github_streams(
    *,
    embedding_client,
    github_store,
    github_token: str,
    bounds: GithubFetchBounds,
) -> GithubIngestResult:
    github_client = GithubApiClient(token=github_token)
    parent_cache = _GithubParentCache(repo=bounds.repo)
    progress = {
        stream_name: _MutableStreamProgress(stream_name=stream_name)
        for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS
    }

    for stream_name in SUPPORTED_GITHUB_BOOTSTRAP_STREAMS:
        page_number = 1
        while True:
            payloads, has_next_page = github_client.fetch_stream_page(
                stream_name,
                bounds=bounds,
                page_number=page_number,
            )
            batch = _prepare_page_batch(
                stream_name,
                page_number=page_number,
                repo=bounds.repo,
                payloads=payloads,
                embedding_client=embedding_client,
                github_client=github_client,
                parent_cache=parent_cache,
                has_next_page=has_next_page,
                truncated_by_bound=False,
            )
            write_result = write_github_page_batch(batch, github_store=github_store)
            progress[stream_name].observe_batch(batch, write_result)
            if not has_next_page:
                break
            page_number += 1

    frozen_progress = {
        stream_name: stream_progress.freeze()
        for stream_name, stream_progress in progress.items()
    }
    return GithubIngestResult(
        repo=bounds.repo,
        bounds=bounds,
        stream_progress=frozen_progress,
        issues_written=sum(item.issues_written for item in frozen_progress.values()),
        issue_comments_written=sum(item.issue_comments_written for item in frozen_progress.values()),
        prs_written=sum(item.prs_written for item in frozen_progress.values()),
        pr_comments_written=sum(item.pr_comments_written for item in frozen_progress.values()),
    )


def _sync_project_changed(
    *,
    config: AgenticConfig,
    embedding_client,
    project_store,
    sync_store: PostgresSyncStateStore,
    baseline: SyncStateRecord,
    attempted_at: datetime,
) -> dict[str, Any]:
    if not config.github_token:
        raise SyncCommandError("GITHUB_TOKEN is required for sync changed project bootstrap refresh")

    starting_cursor = require_project_cursor_baseline(baseline)

    scope_key = baseline.scope_key
    sync_store.record_attempts(
        [
            build_sync_attempt(
                "project",
                scope_key,
                attempted_at=attempted_at,
                metadata={
                    "project_owner": DEFAULT_PROJECT_OWNER,
                    "project_number": DEFAULT_PROJECT_NUMBER,
                },
            )
        ]
    )

    try:
        result = ingest_project_items(
            embedding_client=embedding_client,
            project_store=project_store,
            bounds=ProjectFetchBounds(
                project_owner=DEFAULT_PROJECT_OWNER,
                project_number=DEFAULT_PROJECT_NUMBER,
                after_cursor=starting_cursor,
            ),
            github_token=config.github_token,
        )
    except Exception as error:
        sync_store.record_failures(
            [
                build_sync_failure(
                    "project",
                    scope_key,
                    attempted_at=attempted_at,
                    error=error,
                    metadata={
                        "project_owner": DEFAULT_PROJECT_OWNER,
                        "project_number": DEFAULT_PROJECT_NUMBER,
                    },
                )
            ]
        )
        raise

    state = build_project_sync_state(
        result,
        attempted_at=attempted_at,
        succeeded_at=datetime.now(timezone.utc),
    )
    sync_store.upsert_sync_states([state])
    return {
        "starting_cursor": starting_cursor,
        "final_cursor": result.final_cursor,
        "rows_written": result.rows_written,
        "pages_fetched": result.pages_fetched,
    }


def _run_sync_placeholder(args) -> int:
    name = args.placeholder_name
    print(
        f"sync {name} is not implemented in task-604. "
        f"Use `agentic-kb sync changed` for the post-import bootstrap path today; {SYNC_TASK_GUIDANCE[name]}"
    )
    return 2
