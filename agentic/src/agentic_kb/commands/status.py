from __future__ import annotations

import json
import socket
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable
from urllib.error import URLError
from urllib.parse import urljoin
from urllib.request import urlopen

from agentic_kb.config import AgenticConfig, parse_database_endpoint
from agentic_kb.search.config import list_search_entity_configs


WORKSPACE_PATH = "/workspace"
SNAPSHOTS_PATH = "/workspace/agentic/snapshots"
EXPECTED_MIGRATION_VERSIONS: tuple[int, ...] = (1, 2, 3)
EXPECTED_AGENTIC_TABLES: tuple[str, ...] = (
    "agentic.kb_schema_migrations",
    "agentic.kb_documents",
    "agentic.kb_code_chunks",
    "agentic.kb_github_issues",
    "agentic.kb_github_issue_comments",
    "agentic.kb_github_prs",
    "agentic.kb_github_pr_comments",
    "agentic.kb_project_items",
    "agentic.kb_sync_state",
    "agentic.kb_snapshot_manifest",
)


@dataclass(frozen=True)
class CheckResult:
    name: str
    ok: bool
    detail: str


@dataclass(frozen=True)
class SyncSourceSummary:
    source_name: str
    row_count: int
    error_count: int
    last_attempted_at: datetime | None
    last_succeeded_at: datetime | None


@dataclass(frozen=True)
class DatabaseInspection:
    current_database: str
    applied_versions: tuple[int, ...]
    tables: tuple[str, ...]
    indexes: tuple[str, ...]
    row_counts: dict[str, int]
    sync_summaries: tuple[SyncSourceSummary, ...]


@dataclass(frozen=True)
class StatusReport:
    healthcheck: bool
    ok: bool
    config_items: tuple[CheckResult, ...]
    environment_items: tuple[CheckResult, ...]
    dependency_items: tuple[CheckResult, ...]
    database_items: tuple[CheckResult, ...]
    notes: tuple[str, ...]


class StatusCommandError(RuntimeError):
    pass


def run_status(args) -> int:
    config = AgenticConfig.from_env()
    report = collect_status_report(config, healthcheck=bool(args.healthcheck))
    print_status_report(report)
    return 0 if report.ok or not args.healthcheck else 1


def collect_status_report(config: AgenticConfig, healthcheck: bool) -> StatusReport:
    config_items = list(_config_results(config))
    dependency_items = list(_dependency_results(config, healthcheck=healthcheck))
    environment_items = list(_environment_results())
    database_items = list(_database_results(config, healthcheck=healthcheck))
    ok = all(
        item.ok
        for item in config_items
        + environment_items
        + dependency_items
        + database_items
    )
    notes = (
        (
            "healthcheck mode skips schema inspection and only enforces runtime config plus dependency reachability"
        ),
        (
            "snapshot import is a destructive agentic-schema restore and requires an explicit --yes acknowledgement"
            if not healthcheck
            else "normal status includes schema, index, row-count, and sync-state inspection when DATABASE_URL is usable"
        ),
    )

    return StatusReport(
        healthcheck=healthcheck,
        ok=ok,
        config_items=tuple(config_items),
        environment_items=tuple(environment_items),
        dependency_items=tuple(dependency_items),
        database_items=tuple(database_items),
        notes=notes,
    )


def inspect_database(database_url: str) -> DatabaseInspection:
    psycopg = _load_psycopg()
    try:
        with psycopg.connect(database_url) as connection:
            with connection.cursor() as cursor:
                current_database = _fetch_current_database(cursor)
                applied_versions = _fetch_applied_versions(cursor)
                tables = _fetch_tables(cursor)
                indexes = _fetch_indexes(cursor)
                row_counts = _fetch_searchable_row_counts(cursor, tables)
                sync_summaries = _fetch_sync_summaries(cursor, tables)
    except Exception as error:  # pragma: no cover - exercised via caller-facing tests
        raise StatusCommandError(str(error)) from error

    return DatabaseInspection(
        current_database=current_database,
        applied_versions=applied_versions,
        tables=tables,
        indexes=indexes,
        row_counts=row_counts,
        sync_summaries=sync_summaries,
    )


def build_database_items(
    inspection: DatabaseInspection,
    *,
    endpoint_detail: str,
) -> tuple[CheckResult, ...]:
    searchable_tables = expected_searchable_tables()
    expected_indexes = expected_searchable_indexes()
    table_set = set(inspection.tables)
    index_set = set(inspection.indexes)
    version_set = set(inspection.applied_versions)

    missing_versions = _missing_items(EXPECTED_MIGRATION_VERSIONS, inspection.applied_versions)
    unexpected_versions = sorted(version for version in version_set if version not in EXPECTED_MIGRATION_VERSIONS)
    migration_ok = not missing_versions and not unexpected_versions
    migration_detail_parts = []
    if inspection.applied_versions:
        migration_detail_parts.append(
            f"found {', '.join(str(version) for version in inspection.applied_versions)}"
        )
    else:
        migration_detail_parts.append("found none")
    if missing_versions:
        migration_detail_parts.append(
            f"missing {', '.join(str(version) for version in missing_versions)}"
        )
    if unexpected_versions:
        migration_detail_parts.append(
            f"unexpected {', '.join(str(version) for version in unexpected_versions)}"
        )

    missing_tables = _missing_items(EXPECTED_AGENTIC_TABLES, inspection.tables)
    missing_searchable_tables = _missing_items(searchable_tables, inspection.tables)
    missing_indexes = _missing_items(expected_indexes, inspection.indexes)

    items = [
        CheckResult(
            name="database connection",
            ok=True,
            detail=f"connected to {inspection.current_database} via {endpoint_detail}",
        ),
        CheckResult(
            name="schema migrations",
            ok=migration_ok,
            detail="; ".join(migration_detail_parts),
        ),
        CheckResult(
            name="agentic tables",
            ok=not missing_tables,
            detail=_presence_detail(EXPECTED_AGENTIC_TABLES, missing_tables),
        ),
        CheckResult(
            name="searchable tables",
            ok=not missing_searchable_tables,
            detail=_presence_detail(searchable_tables, missing_searchable_tables),
        ),
        CheckResult(
            name="search indexes",
            ok=not missing_indexes,
            detail=_presence_detail(expected_indexes, missing_indexes),
        ),
    ]

    for table_name in searchable_tables:
        if table_name in inspection.row_counts:
            items.append(
                CheckResult(
                    name=f"rows {table_name}",
                    ok=True,
                    detail=str(inspection.row_counts[table_name]),
                )
            )
        else:
            items.append(
                CheckResult(
                    name=f"rows {table_name}",
                    ok=False,
                    detail="unavailable because the table is missing",
                )
            )

    sync_state_table = "agentic.kb_sync_state"
    if sync_state_table not in table_set:
        items.append(
            CheckResult(
                name="sync state summary",
                ok=False,
                detail="unavailable because agentic.kb_sync_state is missing",
            )
        )
    elif not inspection.sync_summaries:
        items.append(
            CheckResult(
                name="sync state summary",
                ok=True,
                detail="0 rows recorded",
            )
        )
    else:
        total_rows = sum(summary.row_count for summary in inspection.sync_summaries)
        items.append(
            CheckResult(
                name="sync state summary",
                ok=True,
                detail=f"{total_rows} rows across {len(inspection.sync_summaries)} source groups",
            )
        )
        for summary in inspection.sync_summaries:
            items.append(
                CheckResult(
                    name=f"sync {summary.source_name}",
                    ok=True,
                    detail=(
                        f"rows={summary.row_count}, errors={summary.error_count}, "
                        f"last_attempted={_format_timestamp(summary.last_attempted_at)}, "
                        f"last_succeeded={_format_timestamp(summary.last_succeeded_at)}"
                    ),
                )
            )

    return tuple(items)


def expected_searchable_tables() -> tuple[str, ...]:
    return tuple(config.table_name for config in list_search_entity_configs())


def expected_searchable_indexes() -> tuple[str, ...]:
    names: list[str] = []
    for config in list_search_entity_configs():
        short_name = config.table_name.split(".", 1)[1]
        names.append(f"{short_name}_bm25_idx")
        names.append(f"{short_name}_embedding_hnsw_idx")
    return tuple(names)


def print_status_report(report: StatusReport) -> None:
    mode = "healthcheck" if report.healthcheck else "status"
    state = "ok" if report.ok else "degraded"
    print(f"agentic-kb {mode}: {state}")

    _print_section("config", report.config_items)
    _print_section("environment", report.environment_items)
    _print_section("dependencies", report.dependency_items)
    if report.database_items:
        _print_section("database", report.database_items)

    print("notes:")
    for note in report.notes:
        print(f"- {note}")


def _print_section(title: str, items: Iterable[CheckResult]) -> None:
    print(f"{title}:")
    for item in items:
        marker = "ok" if item.ok else "warn"
        print(f"- [{marker}] {item.name}: {item.detail}")


def _config_results(config: AgenticConfig) -> Iterable[CheckResult]:
    runtime_errors = set(config.runtime_errors())

    database_detail = config.database_url or "not set"
    if config.database_url:
        try:
            endpoint = parse_database_endpoint(config.database_url)
            database_detail = f"{endpoint.host}:{endpoint.port}/{endpoint.database}"
        except ValueError as error:
            database_detail = str(error)

    yield CheckResult(
        name="DATABASE_URL",
        ok="DATABASE_URL is required for service mode and healthchecks" not in runtime_errors
        and not database_detail.startswith("DATABASE_URL must"),
        detail=database_detail,
    )

    yield CheckResult(
        name="OLLAMA_BASE_URL",
        ok="OLLAMA_BASE_URL is required for service mode and healthchecks" not in runtime_errors,
        detail=config.ollama_base_url,
    )

    yield CheckResult(
        name="OLLAMA_EMBED_MODEL",
        ok="OLLAMA_EMBED_MODEL is required for service mode and healthchecks" not in runtime_errors,
        detail=config.ollama_embed_model,
    )

    github_detail = "present" if config.github_token else "absent (optional for status and snapshot commands)"
    yield CheckResult(name="GITHUB_TOKEN", ok=True, detail=github_detail)


def _environment_results() -> Iterable[CheckResult]:
    workspace_present = _path_exists(WORKSPACE_PATH)
    snapshots_present = _path_exists(SNAPSHOTS_PATH)

    yield CheckResult(
        name="workspace mount",
        ok=workspace_present,
        detail=f"{'present' if workspace_present else 'missing'} at {WORKSPACE_PATH}",
    )
    yield CheckResult(
        name="snapshots mount",
        ok=snapshots_present,
        detail=f"{'present' if snapshots_present else 'missing'} at {SNAPSHOTS_PATH}",
    )


def _dependency_results(config: AgenticConfig, healthcheck: bool) -> Iterable[CheckResult]:
    if not config.database_url:
        yield CheckResult(
            name="paradedb tcp",
            ok=not healthcheck,
            detail="skipped because DATABASE_URL is not set",
        )
    else:
        try:
            endpoint = parse_database_endpoint(config.database_url)
            _check_tcp(endpoint.host, endpoint.port)
            yield CheckResult(
                name="paradedb tcp",
                ok=True,
                detail=f"reachable at {endpoint.host}:{endpoint.port}",
            )
        except (OSError, ValueError) as error:
            yield CheckResult(name="paradedb tcp", ok=not healthcheck, detail=str(error))

    ollama_ok, ollama_detail, model_ok, model_detail = _check_ollama(config)
    yield CheckResult(name="ollama api", ok=ollama_ok or not healthcheck, detail=ollama_detail)
    yield CheckResult(
        name="ollama embed model",
        ok=model_ok or not healthcheck,
        detail=model_detail,
    )


def _database_results(config: AgenticConfig, healthcheck: bool) -> Iterable[CheckResult]:
    if healthcheck:
        return ()

    if not config.database_url:
        return (
            CheckResult(
                name="database inspection",
                ok=False,
                detail="skipped because DATABASE_URL is not set",
            ),
        )

    try:
        endpoint = parse_database_endpoint(config.database_url)
        endpoint_detail = f"{endpoint.host}:{endpoint.port}/{endpoint.database}"
    except ValueError as error:
        return (CheckResult(name="database inspection", ok=False, detail=str(error)),)

    try:
        inspection = inspect_database(config.database_url)
    except StatusCommandError as error:
        return (
            CheckResult(
                name="database inspection",
                ok=False,
                detail=str(error),
            ),
        )

    return build_database_items(inspection, endpoint_detail=endpoint_detail)


def _fetch_current_database(cursor: Any) -> str:
    cursor.execute("SELECT current_database()")
    row = cursor.fetchone()
    return str(row[0]) if row else "unknown"


def _fetch_applied_versions(cursor: Any) -> tuple[int, ...]:
    cursor.execute("SELECT version FROM agentic.kb_schema_migrations ORDER BY version ASC")
    return tuple(int(row[0]) for row in cursor.fetchall())


def _fetch_tables(cursor: Any) -> tuple[str, ...]:
    cursor.execute(
        """
        SELECT table_schema || '.' || table_name
        FROM information_schema.tables
        WHERE table_schema = 'agentic'
        ORDER BY table_name ASC
        """
    )
    return tuple(str(row[0]) for row in cursor.fetchall())


def _fetch_indexes(cursor: Any) -> tuple[str, ...]:
    cursor.execute(
        """
        SELECT indexname
        FROM pg_indexes
        WHERE schemaname = 'agentic'
        ORDER BY indexname ASC
        """
    )
    return tuple(str(row[0]) for row in cursor.fetchall())


def _fetch_searchable_row_counts(cursor: Any, tables: tuple[str, ...]) -> dict[str, int]:
    psycopg = _load_psycopg()
    table_set = set(tables)
    counts: dict[str, int] = {}

    for table_name in expected_searchable_tables():
        if table_name not in table_set:
            continue
        schema_name, bare_table_name = table_name.split(".", 1)
        cursor.execute(
            psycopg.sql.SQL("SELECT COUNT(*) FROM {}.{}").format(
                psycopg.sql.Identifier(schema_name),
                psycopg.sql.Identifier(bare_table_name),
            )
        )
        row = cursor.fetchone()
        counts[table_name] = int(row[0]) if row else 0

    return counts


def _fetch_sync_summaries(cursor: Any, tables: tuple[str, ...]) -> tuple[SyncSourceSummary, ...]:
    if "agentic.kb_sync_state" not in set(tables):
        return ()

    cursor.execute(
        """
        SELECT
            source_name,
            COUNT(*) AS row_count,
            COUNT(*) FILTER (WHERE last_error IS NOT NULL) AS error_count,
            MAX(last_attempted_at) AS last_attempted_at,
            MAX(last_succeeded_at) AS last_succeeded_at
        FROM agentic.kb_sync_state
        GROUP BY source_name
        ORDER BY source_name ASC
        """
    )
    return tuple(
        SyncSourceSummary(
            source_name=str(row[0]),
            row_count=int(row[1]),
            error_count=int(row[2]),
            last_attempted_at=row[3],
            last_succeeded_at=row[4],
        )
        for row in cursor.fetchall()
    )


def _check_tcp(host: str, port: int) -> None:
    with socket.create_connection((host, port), timeout=3):
        return None


def _check_ollama(config: AgenticConfig) -> tuple[bool, str, bool, str]:
    tags_url = urljoin(config.ollama_base_url.rstrip("/") + "/", "api/tags")

    try:
        with urlopen(tags_url, timeout=5) as response:
            payload = json.load(response)
    except (URLError, TimeoutError, json.JSONDecodeError) as error:
        detail = f"unreachable via {tags_url}: {error}"
        return False, detail, False, "embed model check skipped because ollama api is unavailable"

    models = payload.get("models", [])
    model_names = [model.get("name", "") for model in models if isinstance(model, dict)]
    configured = config.ollama_embed_model
    matches = [name for name in model_names if name == configured or name.startswith(f"{configured}:")]

    api_detail = f"reachable via {tags_url}"
    if matches:
        return True, api_detail, True, f"configured model available as {matches[0]}"

    if model_names:
        return True, api_detail, False, f"configured model {configured} not present; found {', '.join(model_names)}"

    return True, api_detail, False, f"configured model {configured} not present; ollama returned no models"


def _path_exists(path: str) -> bool:
    return Path(path).exists()


def _presence_detail(expected: Iterable[Any], missing: Iterable[Any]) -> str:
    expected_values = tuple(str(value) for value in expected)
    missing_values = tuple(str(value) for value in missing)
    if not missing_values:
        return f"all {len(expected_values)} expected entries present"
    return (
        f"missing {len(missing_values)} of {len(expected_values)} expected entries: "
        + ", ".join(missing_values)
    )


def _missing_items(expected: Iterable[Any], actual: Iterable[Any]) -> tuple[Any, ...]:
    actual_set = set(actual)
    return tuple(item for item in expected if item not in actual_set)


def _format_timestamp(value: datetime | None) -> str:
    if value is None:
        return "never"
    return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def _load_psycopg():
    import psycopg

    return psycopg
