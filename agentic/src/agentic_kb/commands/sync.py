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
    DocsIngestResult,
    PostgresDocsStore,
    discover_docs_source_paths,
    embed_prepared_document_drafts,
    is_allowlisted_doc_path,
    plan_docs_updates,
)
from agentic_kb.ingest.github import (
    GithubFetchBounds,
    PostgresGithubStore,
    STREAM_ORDER,
    ingest_github,
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
    from agentic_kb.ingest.code import (
        PostgresCodeChunksStore,
        ingest_code,
        is_supported_code_path,
    )
except ModuleNotFoundError as error:
    if error.name not in {"tree_sitter", "tree_sitter_language_pack"}:
        raise
    PostgresCodeChunksStore = None
    ingest_code = None
    is_supported_code_path = None


UPSTREAM_SINCE_GITHUB_STREAMS = ("issues", "issue_comments")
CLIENT_FILTERED_GITHUB_STREAMS = ("pulls", "review_comments")
SYNC_ALL_ORDER = ("docs", "code", "github", "project")


class SyncCommandError(RuntimeError):
    pass


class SyncAllFailure(SyncCommandError):
    def __init__(
        self,
        *,
        source_name: str,
        partial_results: dict[str, dict[str, Any]],
        cause: Exception,
    ):
        self.source_name = source_name
        self.partial_results = partial_results
        self.cause = cause
        super().__init__(f"sync all stopped on {source_name}: {cause}")


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

    all_parser = subparsers.add_parser("all", help="Sync docs, code, GitHub, and project in order")
    all_parser.set_defaults(handler=run_sync_all)

    docs_parser = subparsers.add_parser("docs", help="Sync the allowlisted docs corpus")
    docs_parser.set_defaults(handler=run_sync_docs)

    code_parser = subparsers.add_parser("code", help="Sync the supported repository code corpus")
    code_parser.set_defaults(handler=run_sync_code)

    github_parser = subparsers.add_parser("github", help="Sync GitHub issues, PRs, and comments")
    github_parser.set_defaults(handler=run_sync_github)

    project_parser = subparsers.add_parser("project", help="Sync GitHub Project 5 items")
    project_parser.set_defaults(handler=run_sync_project)

    changed_parser = subparsers.add_parser(
        "changed",
        help="Sync only incremental changes from existing source baselines",
    )
    changed_parser.set_defaults(handler=run_sync_changed)


def run_sync_docs(args) -> int:
    return _run_single_source_command("docs", sync_docs)


def run_sync_code(args) -> int:
    return _run_single_source_command("code", sync_code)


def run_sync_github(args) -> int:
    return _run_single_source_command("github", sync_github)


def run_sync_project(args) -> int:
    return _run_single_source_command("project", sync_project)


def run_sync_changed(args) -> int:
    return _run_single_source_command("changed", sync_changed)


def run_sync_all(args) -> int:
    try:
        config = AgenticConfig.from_env()
        result = sync_all(Path.cwd(), config=config)
    except SyncAllFailure as error:
        for source_name in SYNC_ALL_ORDER:
            source_result = error.partial_results.get(source_name)
            if source_result is None:
                break
            for line in format_sync_source_output(source_name, source_result):
                print(line)
        print_stderr(str(error))
        return 1
    except SyncCommandError as error:
        print_stderr(f"sync all failed: {error}")
        return 1
    except Exception as error:
        print_stderr(f"sync all failed: {error}")
        return 1

    for source_name in SYNC_ALL_ORDER:
        for line in format_sync_source_output(source_name, result[source_name]):
            print(line)
    return 0


def _run_single_source_command(command_name: str, handler) -> int:
    try:
        config = AgenticConfig.from_env()
        result = handler(Path.cwd(), config=config)
    except SyncCommandError as error:
        print_stderr(f"sync {command_name} failed: {error}")
        return 1
    except Exception as error:
        print_stderr(f"sync {command_name} failed: {error}")
        return 1

    for line in format_sync_source_output(command_name, result):
        print(line)
    return 0


def sync_all(workspace_root: str | Path, *, config: AgenticConfig) -> dict[str, dict[str, Any]]:
    root = Path(workspace_root).resolve()
    results: dict[str, dict[str, Any]] = {}

    for source_name, handler in (
        ("docs", sync_docs),
        ("code", sync_code),
        ("github", sync_github),
        ("project", sync_project),
    ):
        try:
            results[source_name] = handler(root, config=config)
        except Exception as error:
            raise SyncAllFailure(
                source_name=source_name,
                partial_results=results,
                cause=error,
            ) from error

    return results


def sync_docs(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync docs")

    root = Path(workspace_root).resolve()
    attempted_at = datetime.now(timezone.utc)
    succeeded_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)
    repo_commit_hash = get_head_commit(root)
    discovered_paths = tuple(discover_docs_source_paths(root))
    discovered_path_set = set(discovered_paths)
    scope_key = repo_scope_key(DEFAULT_SYNC_REPO)

    with PostgresDocsStore.from_database_url(config.database_url) as docs_store:
        with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
            sync_store.record_attempts(
                [build_sync_attempt("docs", scope_key, attempted_at=attempted_at, metadata={"repo": DEFAULT_SYNC_REPO})]
            )
            try:
                existing_paths = docs_store.list_document_paths()
                stale_paths = tuple(
                    path
                    for path in existing_paths
                    if is_allowlisted_doc_path(path) and path not in discovered_path_set
                )
                plan = plan_docs_updates(
                    root,
                    docs_store=docs_store,
                    source_paths=discovered_paths,
                    repo_commit_hash=repo_commit_hash,
                )
                documents = embed_prepared_document_drafts(
                    plan.drafts,
                    embedding_client=embedding_client,
                )
                if plan.updated_paths:
                    docs_store.replace_documents_for_paths(plan.updated_paths, documents)
                if stale_paths:
                    docs_store.delete_documents_for_paths(stale_paths)
            except Exception as error:
                sync_store.record_failures(
                    [build_sync_failure("docs", scope_key, attempted_at=attempted_at, error=error, metadata={"repo": DEFAULT_SYNC_REPO})]
                )
                raise

            result = DocsIngestResult(
                source_paths=plan.updated_paths,
                processed_count=len(documents),
                repo_commit_hash=repo_commit_hash,
                candidate_paths=plan.candidate_paths,
                updated_paths=plan.updated_paths,
                skipped_paths=plan.skipped_paths,
                deleted_paths=stale_paths,
            )
            state = build_docs_sync_state(
                result,
                attempted_at=attempted_at,
                succeeded_at=succeeded_at,
                repo=DEFAULT_SYNC_REPO,
            )
            sync_store.upsert_sync_states([state])

    return {
        "mode": "explicit",
        "candidate_paths": result.candidate_paths,
        "source_paths": result.source_paths,
        "updated_paths": result.updated_paths,
        "skipped_paths": result.skipped_paths,
        "processed_count": result.processed_count,
        "deleted_paths": stale_paths,
        "repo_commit_hash": repo_commit_hash,
    }


def sync_code(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync code")
    _require_code_ingest_dependencies("sync code")

    root = Path(workspace_root).resolve()
    attempted_at = datetime.now(timezone.utc)
    succeeded_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)
    repo_commit_hash = get_head_commit(root)
    scope_key = repo_scope_key(DEFAULT_SYNC_REPO)

    with PostgresCodeChunksStore.from_database_url(config.database_url) as code_store:
        with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
            sync_store.record_attempts(
                [build_sync_attempt("code", scope_key, attempted_at=attempted_at, metadata={"repo": DEFAULT_SYNC_REPO})]
            )
            try:
                result = ingest_code(
                    root,
                    embedding_client=embedding_client,
                    code_store=code_store,
                    repo_commit_hash=repo_commit_hash,
                    run_mode="full_repository",
                    prune_missing=True,
                )
            except Exception as error:
                sync_store.record_failures(
                    [build_sync_failure("code", scope_key, attempted_at=attempted_at, error=error, metadata={"repo": DEFAULT_SYNC_REPO})]
                )
                raise

            state = build_code_sync_state(
                result,
                attempted_at=attempted_at,
                succeeded_at=succeeded_at,
                repo=DEFAULT_SYNC_REPO,
            )
            sync_store.upsert_sync_states([state])

    return {
        "mode": "explicit",
        "source_paths": result.source_paths,
        "processed_file_count": result.processed_file_count,
        "chunk_count": result.chunk_count,
        "repo_commit_hash": result.repo_commit_hash,
    }


def sync_github(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    del workspace_root
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync github")
    if not config.github_token:
        raise SyncCommandError("GITHUB_TOKEN is required for sync github")

    attempted_at = datetime.now(timezone.utc)
    succeeded_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)

    with PostgresGithubStore.from_database_url(config.database_url) as github_store:
        with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
            baselines = load_github_sync_baselines(sync_store, allow_empty=True)
            updated_since, mode = derive_github_updated_since_for_explicit_sync(baselines)
            sync_store.record_attempts(
                [
                    build_sync_attempt(
                        "github",
                        github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
                        attempted_at=attempted_at,
                        metadata={"repo": DEFAULT_SYNC_REPO, "stream_name": stream_name},
                    )
                    for stream_name in STREAM_ORDER
                ]
            )
            try:
                result = ingest_github(
                    embedding_client=embedding_client,
                    github_store=github_store,
                    bounds=GithubFetchBounds(repo=DEFAULT_SYNC_REPO, updated_since=updated_since),
                    github_token=config.github_token,
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
                        for stream_name in STREAM_ORDER
                    ]
                )
                raise

            states = merge_github_sync_states(
                build_github_sync_state_updates(
                    result,
                    attempted_at=attempted_at,
                    succeeded_at=succeeded_at,
                ),
                baselines=baselines,
                succeeded_at=succeeded_at,
                seed_empty_streams=mode == "initial",
            )
            sync_store.upsert_sync_states(states)

    return {
        "mode": mode,
        "updated_since": updated_since,
        "stream_progress": result.stream_progress,
        "upstream_since_streams": UPSTREAM_SINCE_GITHUB_STREAMS,
        "client_filtered_streams": CLIENT_FILTERED_GITHUB_STREAMS,
    }


def sync_project(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    del workspace_root
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync project")
    if not config.github_token:
        raise SyncCommandError("GITHUB_TOKEN is required for sync project")

    attempted_at = datetime.now(timezone.utc)
    succeeded_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)

    with PostgresProjectItemsStore.from_database_url(config.database_url) as project_store:
        with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
            baseline = load_project_sync_baseline(sync_store, allow_empty=True)
            starting_cursor, mode = derive_project_cursor_for_explicit_sync(baseline)
            scope_key = project_scope_key(DEFAULT_PROJECT_OWNER, DEFAULT_PROJECT_NUMBER)
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

            state = merge_project_sync_state(
                build_project_sync_state(result, attempted_at=attempted_at, succeeded_at=succeeded_at),
                baseline=baseline,
            )
            sync_store.upsert_sync_states([state])

    return {
        "mode": mode,
        "starting_cursor": starting_cursor,
        "final_cursor": result.final_cursor,
        "pages_fetched": result.pages_fetched,
        "rows_written": result.rows_written,
        "limitation": "cursor_continuation_only",
    }


def sync_changed(
    workspace_root: str | Path,
    *,
    config: AgenticConfig,
) -> dict[str, Any]:
    if not config.database_url:
        raise SyncCommandError("DATABASE_URL is required for sync changed")
    if not config.github_token:
        raise SyncCommandError("GITHUB_TOKEN is required for sync changed")
    _require_code_ingest_dependencies("sync changed")

    root = Path(workspace_root).resolve()
    attempted_at = datetime.now(timezone.utc)
    succeeded_at = datetime.now(timezone.utc)
    embedding_client = OllamaEmbeddingClient.from_config(config)

    with PostgresSyncStateStore.from_database_url(config.database_url) as sync_store:
        baselines = load_required_incremental_baselines(sync_store)
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
                succeeded_at=succeeded_at,
            )

    return {
        "mode": "incremental",
        "docs": docs_result,
        "code": code_result,
        "github": github_result,
        "project": project_result,
        "head_commit": get_head_commit(root),
    }


def format_sync_source_output(command_name: str, result: dict[str, Any]) -> list[str]:
    if command_name == "docs":
        return [
            "sync docs completed",
            (
                "docs: "
                f"mode={result['mode']}, "
                f"updated={len(result.get('updated_paths', result.get('source_paths', ())))}, "
                f"skipped={len(result.get('skipped_paths', ()))}, "
                f"processed={result['processed_count']}, "
                f"deleted={len(result.get('deleted_paths', ()))}, "
                f"commit={result['repo_commit_hash']}"
            ),
        ]
    if command_name == "code":
        return [
            "sync code completed",
            (
                "code: "
                f"mode={result['mode']}, "
                f"files={result['processed_file_count']}, "
                f"chunks={result['chunk_count']}, "
                f"commit={result['repo_commit_hash']}"
            ),
        ]
    if command_name == "github":
        return _format_github_output("sync github completed", result)
    if command_name == "project":
        return [
            "sync project completed",
            (
                "project: "
                f"mode={result['mode']}, "
                f"cursor={result['starting_cursor'] or '<start>'} -> {result['final_cursor'] or '<unchanged>'}, "
                f"pages={result['pages_fetched']}, rows={result['rows_written']}"
            ),
            "note: Project sync is cursor continuation only and can still miss edits to already-seen items.",
        ]
    if command_name == "changed":
        lines = ["sync changed completed"]
        lines.extend(_format_changed_local_output("docs", result["docs"]))
        lines.extend(_format_changed_local_output("code", result["code"]))
        lines.extend(_format_github_output("github incremental refresh completed", result["github"]))
        lines.append(
            "project: "
            f"cursor={result['project']['starting_cursor'] or '<start>'} -> {result['project']['final_cursor'] or '<unchanged>'}, "
            f"pages={result['project']['pages_fetched']}, rows={result['project']['rows_written']}"
        )
        lines.append(
            "note: Project sync remains cursor continuation only and can still miss edits to already-seen items."
        )
        return lines
    raise SyncCommandError(f"unsupported sync output formatter for {command_name}")


def _format_changed_local_output(source_name: str, result: dict[str, Any]) -> list[str]:
    count_key = "processed_count" if source_name == "docs" else "processed_file_count"
    return [
        (
            f"{source_name}: "
            f"updated={len(result.get('updated_paths', result['changed_paths']))}, "
            f"skipped={len(result.get('skipped_paths', ()))}, "
            f"deleted={len(result['deleted_paths'])}, "
            f"{count_key}={result[count_key]}, "
            f"baseline={result['baseline_commit']} -> {result['repo_commit_hash']}"
        )
    ]


def _format_github_output(title: str, result: dict[str, Any]) -> list[str]:
    lines = [title]
    lines.append(
        "github: "
        f"mode={result['mode']}, "
        f"updated_since={_format_timestamp(result['updated_since']) or '<full>'}"
    )
    for stream_name in STREAM_ORDER:
        progress = result["stream_progress"][stream_name]
        lines.append(
            f"github {stream_name}: pages={progress.pages_fetched}, hit_bound={progress.hit_bound}, "
            f"latest={_format_timestamp(progress.latest_source_updated_at) or '<none>'}"
        )
    lines.append(
        "note: GitHub uses one four-stream ingest; upstream `since` applies only to issues and issue_comments."
    )
    lines.append(
        "note: pulls and review_comments reuse the same lower bound but rely on ordered fetch plus client-side filtering."
    )
    return lines


def load_required_incremental_baselines(sync_store: PostgresSyncStateStore) -> dict[str, Any]:
    docs_record = require_successful_repo_baseline(sync_store, "docs")
    code_record = require_successful_repo_baseline(sync_store, "code")
    project_record = require_successful_project_baseline(sync_store)
    github_records = load_github_sync_baselines(sync_store, allow_empty=False)

    if github_records is None:
        raise SyncCommandError("sync changed requires existing successful GitHub stream baselines; run `agentic-kb sync github` first")
    for stream_name in GITHUB_STREAM_NAMES:
        record = github_records[stream_name]
        require_successful_github_watermark(record, stream_name)

    return {
        "docs": docs_record,
        "code": code_record,
        "project": project_record,
        "github": github_records,
    }


def require_successful_repo_baseline(
    sync_store: PostgresSyncStateStore,
    source_name: str,
) -> SyncStateRecord:
    scope_key = repo_scope_key(DEFAULT_SYNC_REPO)
    record = sync_store.get_sync_state(source_name, scope_key)
    if record is None or record.last_succeeded_at is None or not record.repo_commit_hash:
        raise SyncCommandError(
            f"sync changed requires an existing successful {source_name} baseline with repo_commit_hash; "
            f"run `agentic-kb sync {source_name}` or `agentic-kb sync all` first"
        )
    return record


def require_successful_project_baseline(sync_store: PostgresSyncStateStore) -> SyncStateRecord:
    scope_key = project_scope_key(DEFAULT_PROJECT_OWNER, DEFAULT_PROJECT_NUMBER)
    record = sync_store.get_sync_state("project", scope_key)
    if record is None or record.last_succeeded_at is None:
        raise SyncCommandError(
            "sync changed requires an existing successful project baseline; run `agentic-kb sync project` or `agentic-kb sync all` first"
        )
    cursor = project_cursor(record)
    if cursor is None or not cursor.strip():
        raise SyncCommandError(
            "sync changed requires an existing successful project baseline with non-empty cursor_text; "
            "run `agentic-kb sync project` first"
        )
    return record


def load_github_sync_baselines(
    sync_store: PostgresSyncStateStore,
    *,
    allow_empty: bool,
) -> dict[str, SyncStateRecord] | None:
    records: dict[str, SyncStateRecord] = {}
    missing_streams: list[str] = []
    for stream_name in GITHUB_STREAM_NAMES:
        record = sync_store.get_sync_state("github", github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO))
        if record is None:
            missing_streams.append(stream_name)
            continue
        records[stream_name] = record

    if not records and allow_empty:
        return None
    if missing_streams:
        raise SyncCommandError(
            "GitHub sync state is incomplete; expected all four stream rows (issues, pulls, issue_comments, review_comments). "
            "Run `agentic-kb sync github` to seed them."
        )
    return records


def derive_github_updated_since_for_explicit_sync(
    baselines: dict[str, SyncStateRecord] | None,
) -> tuple[datetime | None, str]:
    if baselines is None:
        return None, "initial"
    watermarks = []
    for stream_name in GITHUB_STREAM_NAMES:
        record = baselines[stream_name]
        require_successful_github_watermark(record, stream_name, command_name="sync github")
        watermark = github_stream_updated_since(record)
        if watermark is None:
            raise SyncCommandError(
                f"sync github requires an existing successful github baseline with watermark for {stream_name}; "
                "run `agentic-kb sync github` first"
            )
        watermarks.append(watermark)
    return min(watermarks), "incremental"


def require_successful_github_watermark(
    record: SyncStateRecord,
    stream_name: str,
    *,
    command_name: str = "sync changed",
) -> None:
    if record.last_succeeded_at is None or github_stream_updated_since(record) is None:
        raise SyncCommandError(
            f"{command_name} requires an existing successful github baseline with watermark for {stream_name}; "
            "run `agentic-kb sync github` first"
        )


def derive_project_cursor_for_explicit_sync(
    baseline: SyncStateRecord | None,
) -> tuple[str | None, str]:
    if baseline is None:
        return None, "initial"
    if baseline.last_succeeded_at is None:
        raise SyncCommandError(
            "sync project requires a successful existing baseline before cursor continuation can resume"
        )
    cursor = project_cursor(baseline)
    if cursor is None or not cursor.strip():
        raise SyncCommandError(
            "sync project found an incomplete existing baseline without cursor_text; reseed it with a clean initial `agentic-kb sync project` run"
        )
    return cursor, "incremental"


def load_project_sync_baseline(
    sync_store: PostgresSyncStateStore,
    *,
    allow_empty: bool,
) -> SyncStateRecord | None:
    record = sync_store.get_sync_state(
        "project",
        project_scope_key(DEFAULT_PROJECT_OWNER, DEFAULT_PROJECT_NUMBER),
    )
    if record is None and allow_empty:
        return None
    return record


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
        if status in {"C", "R"} and len(parts) >= 3:
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
            f"baseline commit {commit_hash} is not available in the local clone; fetch full history or reseed the KB"
        ) from error


def _run_git_command(workspace_root: Path, args: Sequence[str]) -> str:
    try:
        completed = run(
            ["git", "-c", f"safe.directory={workspace_root}", *args],
            check=True,
            capture_output=True,
            text=True,
            cwd=workspace_root,
        )
    except FileNotFoundError as error:
        raise SyncCommandError("git is required for sync commands") from error
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
        plan = plan_docs_updates(
            workspace_root,
            docs_store=docs_store,
            source_paths=delta.changed_paths,
            repo_commit_hash=delta.head_commit,
        )
        documents = embed_prepared_document_drafts(
            plan.drafts,
            embedding_client=embedding_client,
        )
        if plan.updated_paths:
            docs_store.replace_documents_for_paths(plan.updated_paths, documents)
        if delta.deleted_paths:
            docs_store.delete_documents_for_paths(delta.deleted_paths)
    except Exception as error:
        sync_store.record_failures(
            [build_sync_failure("docs", scope_key, attempted_at=attempted_at, error=error, metadata={"repo": DEFAULT_SYNC_REPO})]
        )
        raise

    result = DocsIngestResult(
        source_paths=plan.updated_paths,
        processed_count=len(documents),
        repo_commit_hash=delta.head_commit,
        candidate_paths=plan.candidate_paths,
        updated_paths=plan.updated_paths,
        skipped_paths=plan.skipped_paths,
        deleted_paths=delta.deleted_paths,
    )
    state = build_docs_sync_state(
        result,
        attempted_at=attempted_at,
        succeeded_at=datetime.now(timezone.utc),
        repo=DEFAULT_SYNC_REPO,
    )
    sync_store.upsert_sync_states([state])
    return {
        "changed_paths": delta.changed_paths,
        "candidate_paths": result.candidate_paths,
        "updated_paths": result.updated_paths,
        "skipped_paths": result.skipped_paths,
        "deleted_paths": delta.deleted_paths,
        "processed_count": result.processed_count,
        "repo_commit_hash": state.repo_commit_hash,
        "baseline_commit": delta.baseline_commit,
    }


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
    succeeded_at = datetime.now(timezone.utc)
    updated_since = min(
        github_stream_updated_since(baselines["github"][stream_name])
        for stream_name in GITHUB_STREAM_NAMES
    )

    sync_store.record_attempts(
        [
            build_sync_attempt(
                "github",
                github_scope_key(stream_name, repo=DEFAULT_SYNC_REPO),
                attempted_at=attempted_at,
                metadata={"repo": DEFAULT_SYNC_REPO, "stream_name": stream_name},
            )
            for stream_name in STREAM_ORDER
        ]
    )

    try:
        github_result = ingest_github(
            embedding_client=embedding_client,
            github_store=github_store,
            bounds=GithubFetchBounds(repo=DEFAULT_SYNC_REPO, updated_since=updated_since),
            github_token=config.github_token,
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
                for stream_name in STREAM_ORDER
            ]
        )
        raise

    states = merge_github_sync_states(
        build_github_sync_state_updates(
            github_result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        ),
        baselines=baselines["github"],
        succeeded_at=succeeded_at,
        seed_empty_streams=False,
    )
    sync_store.upsert_sync_states(states)

    return {
        "mode": "incremental",
        "updated_since": github_result.bounds.updated_since,
        "stream_progress": github_result.stream_progress,
        "upstream_since_streams": UPSTREAM_SINCE_GITHUB_STREAMS,
        "client_filtered_streams": CLIENT_FILTERED_GITHUB_STREAMS,
    }


def _sync_project_changed(
    *,
    config: AgenticConfig,
    embedding_client,
    project_store,
    sync_store: PostgresSyncStateStore,
    baseline: SyncStateRecord,
    attempted_at: datetime,
    succeeded_at: datetime,
) -> dict[str, Any]:
    starting_cursor = project_cursor(baseline)
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

    state = merge_project_sync_state(
        build_project_sync_state(
            result,
            attempted_at=attempted_at,
            succeeded_at=succeeded_at,
        ),
        baseline=baseline,
    )
    sync_store.upsert_sync_states([state])
    return {
        "mode": "incremental",
        "starting_cursor": starting_cursor,
        "final_cursor": result.final_cursor,
        "rows_written": result.rows_written,
        "pages_fetched": result.pages_fetched,
    }


def merge_github_sync_states(
    states: Sequence[PreparedSyncState],
    *,
    baselines: dict[str, SyncStateRecord] | None,
    succeeded_at: datetime,
    seed_empty_streams: bool,
) -> list[PreparedSyncState]:
    merged: list[PreparedSyncState] = []
    for state in states:
        stream_name = state.metadata.get("stream_name")
        baseline = baselines.get(stream_name) if baselines is not None else None
        watermark_timestamp = state.watermark_timestamp
        watermark_text = state.watermark_text
        metadata = dict(state.metadata)
        if watermark_timestamp is None:
            if baseline is not None and baseline.watermark_timestamp is not None:
                watermark_timestamp = baseline.watermark_timestamp
                watermark_text = baseline.watermark_text
            elif seed_empty_streams:
                watermark_timestamp = succeeded_at
                watermark_text = _format_timestamp(succeeded_at)
                metadata["watermark_seeded_from"] = "sync_completed_at"
        merged.append(
            PreparedSyncState(
                id=state.id,
                source_name=state.source_name,
                scope_key=state.scope_key,
                repo_commit_hash=state.repo_commit_hash,
                cursor_text=state.cursor_text,
                watermark_text=watermark_text,
                watermark_timestamp=watermark_timestamp,
                schema_version=state.schema_version,
                last_attempted_at=state.last_attempted_at,
                last_succeeded_at=state.last_succeeded_at,
                last_error=state.last_error,
                metadata=metadata,
            )
        )
    return merged


def merge_project_sync_state(
    state: PreparedSyncState,
    *,
    baseline: SyncStateRecord | None,
) -> PreparedSyncState:
    watermark_timestamp = state.watermark_timestamp
    watermark_text = state.watermark_text
    if watermark_timestamp is None and baseline is not None and baseline.watermark_timestamp is not None:
        watermark_timestamp = baseline.watermark_timestamp
        watermark_text = baseline.watermark_text
    return PreparedSyncState(
        id=state.id,
        source_name=state.source_name,
        scope_key=state.scope_key,
        repo_commit_hash=state.repo_commit_hash,
        cursor_text=state.cursor_text,
        watermark_text=watermark_text,
        watermark_timestamp=watermark_timestamp,
        schema_version=state.schema_version,
        last_attempted_at=state.last_attempted_at,
        last_succeeded_at=state.last_succeeded_at,
        last_error=state.last_error,
        metadata=dict(state.metadata),
    )


def _require_code_ingest_dependencies(command_name: str) -> None:
    if PostgresCodeChunksStore is None or ingest_code is None or is_supported_code_path is None:
        raise SyncCommandError(
            f"{command_name} requires the optional tree-sitter code-ingest dependencies to be installed"
        )


def _format_timestamp(value: datetime | None) -> str | None:
    if value is None:
        return None
    return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
