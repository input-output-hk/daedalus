from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from agentic_kb.config import AgenticConfig, parse_database_endpoint
from agentic_kb.snapshot_manifest import (
    SNAPSHOT_ENTITY_COUNT_KEYS,
    build_snapshot_manifest,
    build_runtime_embedding_contract,
    describe_embedding_contract_mismatches,
    extract_snapshot_embedding_contract,
    normalize_sync_state_records,
    snapshot_manifest_record_fields,
    validate_snapshot_manifest,
)
from agentic_kb.sync.state import (
    DEFAULT_SYNC_REPO,
    PostgresSyncStateStore,
    SyncStateRecord,
)


DEFAULT_SNAPSHOTS_PATH = Path("/workspace/agentic/snapshots")
DEFAULT_SNAPSHOT_PREFIX = "agentic-kb"
AGENTIC_SCHEMA = "agentic"
SNAPSHOT_DUMP_SUFFIX = ".dump"
SNAPSHOT_MANIFEST_SUFFIX = ".manifest.json"
SNAPSHOT_HASH_CHUNK_SIZE = 1024 * 1024
SNAPSHOT_ENTITY_TABLES: tuple[tuple[str, str], ...] = (
    ("documents", "agentic.kb_documents"),
    ("code_chunks", "agentic.kb_code_chunks"),
    ("github_issues", "agentic.kb_github_issues"),
    ("github_issue_comments", "agentic.kb_github_issue_comments"),
    ("github_prs", "agentic.kb_github_prs"),
    ("github_pr_comments", "agentic.kb_github_pr_comments"),
    ("project_items", "agentic.kb_project_items"),
)
SNAPSHOT_IMPORT_STATE_TABLES: tuple[str, ...] = (
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
class SnapshotResult:
    path: Path
    database_name: str
    manifest_path: Path | None = None


@dataclass(frozen=True)
class SnapshotPair:
    dump_path: Path
    manifest_path: Path


@dataclass(frozen=True)
class SnapshotExportMetadata:
    snapshot_name: str
    snapshot_created_at: datetime
    repo_name: str
    docs_commit_hash: str | None
    code_commit_hash: str | None
    embedding_contract: Any
    entity_counts: dict[str, int]
    sync_state: dict[str, Any]


@dataclass(frozen=True)
class SnapshotImportTargetInspection:
    schema_exists: bool
    state_table_row_counts: dict[str, int]

    @property
    def nonempty_tables(self) -> dict[str, int]:
        return {
            table_name: row_count
            for table_name, row_count in self.state_table_row_counts.items()
            if row_count > 0
        }


class SnapshotCommandError(RuntimeError):
    pass


def add_snapshot_subcommands(parser) -> None:
    subparsers = parser.add_subparsers(dest="snapshot_command", required=True)

    export_parser = subparsers.add_parser(
        "export",
        help="Export a custom-format dump of the agentic schema",
    )
    export_parser.add_argument(
        "path",
        nargs="?",
        help="Optional output dump path; defaults to /workspace/agentic/snapshots/<timestamp>.dump",
    )
    export_parser.set_defaults(handler=run_snapshot_export)

    import_parser = subparsers.add_parser(
        "import",
        help="Destructively restore the agentic schema from a custom-format dump",
    )
    import_parser.add_argument(
        "path",
        help="Path to a previously exported snapshot dump or manifest",
    )
    import_parser.add_argument(
        "--yes",
        action="store_true",
        help="Acknowledge that this command drops and recreates the agentic schema",
    )
    import_parser.set_defaults(handler=run_snapshot_import)


def run_snapshot_export(args) -> int:
    try:
        database_url = _database_url_from_env()
        destination = resolve_export_path(args.path)
        result = export_snapshot(database_url, destination)
    except SnapshotCommandError as error:
        print(f"snapshot export failed: {error}")
        return 2

    if result.manifest_path is None:
        print(f"snapshot export complete: {result.path} ({result.database_name})")
    else:
        print(
            "snapshot export complete: "
            f"{result.path} + {result.manifest_path} ({result.database_name})"
        )
    return 0


def run_snapshot_import(args) -> int:
    if not getattr(args, "yes", False):
        print(
            "snapshot import is destructive: it drops and recreates the agentic schema in the target database. "
            "Re-run with --yes against a disposable KB database."
        )
        return 2

    try:
        database_url = _database_url_from_env()
        result = import_snapshot(database_url, args.path, confirmed=True)
    except SnapshotCommandError as error:
        print(f"snapshot import failed: {error}")
        return 2

    if result.manifest_path is None:
        print(f"snapshot import complete: {result.path} ({result.database_name})")
    else:
        print(
            "snapshot import complete: "
            f"{result.path} + {result.manifest_path} ({result.database_name})"
        )
    return 0


def export_snapshot(database_url: str, destination: str | Path) -> SnapshotResult:
    endpoint = _validated_endpoint(database_url)
    output_path = resolve_export_path(destination)
    manifest_path = build_manifest_path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    _ensure_required_binaries("pg_dump")

    metadata: SnapshotExportMetadata | None = None
    manifest: dict[str, Any] | None = None
    psycopg = _load_psycopg()
    connection = psycopg.connect(database_url)
    connection.autocommit = True
    try:
        with connection.cursor() as cursor:
            cursor.execute("BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY")
            metadata = _collect_export_metadata(connection, cursor, output_path=output_path)
            cursor.execute("SELECT pg_export_snapshot()")
            exported_snapshot = cursor.fetchone()
            if not exported_snapshot or not exported_snapshot[0]:
                raise SnapshotCommandError(
                    "failed to export a PostgreSQL snapshot for manifest-consistent export"
                )

            _run_subprocess(build_pg_dump_command(database_url, output_path, snapshot_id=exported_snapshot[0]))
            artifact_size_bytes = output_path.stat().st_size
            artifact_content_hash = compute_file_sha256(output_path)
            manifest = build_snapshot_manifest(
                snapshot_name=metadata.snapshot_name,
                snapshot_created_at=metadata.snapshot_created_at,
                artifact_filename=output_path.name,
                artifact_size_bytes=artifact_size_bytes,
                artifact_content_hash=artifact_content_hash,
                repo_name=metadata.repo_name,
                docs_commit_hash=metadata.docs_commit_hash,
                code_commit_hash=metadata.code_commit_hash,
                embedding_contract=metadata.embedding_contract,
                entity_counts=metadata.entity_counts,
                sync_state=metadata.sync_state,
            )
            _write_manifest_file(manifest_path, manifest)
            cursor.execute("COMMIT")
    except Exception as error:
        try:
            connection.rollback()
        except Exception:
            pass
        _cleanup_partial_export(output_path, manifest_path)
        if isinstance(error, SnapshotCommandError):
            raise
        raise SnapshotCommandError(str(error)) from error
    finally:
        connection.close()

    _persist_snapshot_manifest(
        database_url,
        manifest,
        source_path=str(manifest_path),
    )
    return SnapshotResult(path=output_path, database_name=endpoint.database, manifest_path=manifest_path)


def import_snapshot(
    database_url: str,
    source_path: str | Path,
    *,
    confirmed: bool,
) -> SnapshotResult:
    if not confirmed:
        raise SnapshotCommandError("snapshot import requires explicit destructive confirmation")

    endpoint = _validated_endpoint(database_url)
    pair, manifest = resolve_import_snapshot_pair(source_path)
    _ensure_required_binaries("psql", "pg_restore")
    validate_snapshot_artifact(pair.dump_path, manifest)
    ensure_disposable_import_target(database_url)
    ensure_compatible_snapshot_import(manifest)

    restore_list_path = build_agentic_restore_list(pair.dump_path)
    _run_subprocess(build_drop_schema_command(database_url))
    try:
        _run_subprocess(build_pg_restore_command(database_url, pair.dump_path, restore_list_path))
    finally:
        restore_list_path.unlink(missing_ok=True)

    _persist_snapshot_manifest(
        database_url,
        manifest,
        source_path=str(pair.manifest_path),
        imported_at=_utcnow(),
    )
    return SnapshotResult(
        path=pair.dump_path,
        database_name=endpoint.database,
        manifest_path=pair.manifest_path,
    )


def resolve_export_path(path_value: str | Path | None, *, now: datetime | None = None) -> Path:
    if path_value is None:
        return DEFAULT_SNAPSHOTS_PATH / _default_snapshot_name(now=now)

    path = Path(path_value).expanduser()
    if path.exists() and path.is_dir():
        path = path / _default_snapshot_name(now=now)

    path = _absolute_path(path)
    if not path.name.endswith(SNAPSHOT_DUMP_SUFFIX):
        raise SnapshotCommandError(
            f"snapshot export path must end with {SNAPSHOT_DUMP_SUFFIX}: {path}"
        )
    return path


def resolve_import_path(path_value: str | Path) -> Path:
    path = _absolute_path(Path(path_value).expanduser())
    if not path.exists():
        raise SnapshotCommandError(f"snapshot dump does not exist: {path}")
    if not path.is_file():
        raise SnapshotCommandError(f"snapshot dump is not a file: {path}")
    return path


def resolve_import_snapshot_pair(path_value: str | Path) -> tuple[SnapshotPair, dict[str, Any]]:
    input_path = resolve_import_path(path_value)
    if input_path.name.endswith(SNAPSHOT_MANIFEST_SUFFIX):
        manifest_path = input_path
        manifest = _load_manifest_file(manifest_path)
        dump_path = resolve_dump_path_from_manifest(manifest_path, manifest)
        return SnapshotPair(dump_path=dump_path, manifest_path=manifest_path), manifest

    dump_path = input_path
    manifest_path = derive_manifest_path_from_dump_path(dump_path)
    if not manifest_path.exists():
        raise SnapshotCommandError(f"snapshot manifest does not exist: {manifest_path}")
    if not manifest_path.is_file():
        raise SnapshotCommandError(f"snapshot manifest is not a file: {manifest_path}")
    manifest = _load_manifest_file(manifest_path)
    expected_dump_name = manifest["artifact"]["filename"]
    if expected_dump_name != dump_path.name:
        raise SnapshotCommandError(
            "snapshot manifest artifact filename does not match the provided dump path: "
            f"expected {expected_dump_name}, got {dump_path.name}"
        )
    return SnapshotPair(dump_path=dump_path, manifest_path=manifest_path), manifest


def derive_manifest_path_from_dump_path(dump_path: Path) -> Path:
    if not dump_path.name.endswith(SNAPSHOT_DUMP_SUFFIX):
        raise SnapshotCommandError(
            f"snapshot dump path must end with {SNAPSHOT_DUMP_SUFFIX} for sibling manifest resolution: {dump_path}"
        )
    snapshot_name = snapshot_name_from_dump_path(dump_path)
    return dump_path.with_name(f"{snapshot_name}{SNAPSHOT_MANIFEST_SUFFIX}")


def resolve_dump_path_from_manifest(manifest_path: Path, manifest: dict[str, Any]) -> Path:
    artifact_filename = manifest["artifact"]["filename"]
    if Path(artifact_filename).name != artifact_filename:
        raise SnapshotCommandError(
            "snapshot manifest artifact filename must be a basename-only sibling dump filename"
        )
    if not artifact_filename.endswith(SNAPSHOT_DUMP_SUFFIX):
        raise SnapshotCommandError(
            f"snapshot manifest artifact filename must end with {SNAPSHOT_DUMP_SUFFIX}: {artifact_filename}"
        )
    dump_path = manifest_path.parent / artifact_filename
    if not dump_path.exists():
        raise SnapshotCommandError(f"snapshot dump does not exist: {dump_path}")
    if not dump_path.is_file():
        raise SnapshotCommandError(f"snapshot dump is not a file: {dump_path}")
    return dump_path


def build_manifest_path(dump_path: Path) -> Path:
    return derive_manifest_path_from_dump_path(dump_path)


def snapshot_name_from_dump_path(dump_path: Path) -> str:
    if not dump_path.name.endswith(SNAPSHOT_DUMP_SUFFIX):
        raise SnapshotCommandError(
            f"snapshot dump path must end with {SNAPSHOT_DUMP_SUFFIX}: {dump_path}"
        )
    return dump_path.name[: -len(SNAPSHOT_DUMP_SUFFIX)]


def build_pg_dump_command(
    database_url: str,
    output_path: Path,
    *,
    snapshot_id: str | None = None,
) -> list[str]:
    command = [
        "pg_dump",
        "--format=custom",
        "--compress=6",
        "--no-owner",
        "--no-privileges",
        f"--schema={AGENTIC_SCHEMA}",
        f"--file={output_path}",
    ]
    if snapshot_id is not None:
        command.append(f"--snapshot={snapshot_id}")
    command.append(database_url)
    return command


def build_drop_schema_command(database_url: str) -> list[str]:
    return [
        "psql",
        database_url,
        "--set",
        "ON_ERROR_STOP=1",
        "--command",
        f"DROP SCHEMA IF EXISTS {AGENTIC_SCHEMA} CASCADE;",
    ]


def build_pg_restore_command(database_url: str, dump_path: Path, restore_list_path: Path) -> list[str]:
    return [
        "pg_restore",
        "--dbname",
        database_url,
        "--exit-on-error",
        "--no-owner",
        "--no-privileges",
        f"--use-list={restore_list_path}",
        str(dump_path),
    ]


def build_agentic_restore_list(dump_path: Path) -> Path:
    toc_text = _run_subprocess_capture(build_pg_restore_list_command(dump_path))
    toc_lines = filter_agentic_restore_toc(toc_text)
    handle = tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        prefix="agentic-restore-",
        suffix=".list",
        delete=False,
    )
    try:
        with handle:
            handle.write("\n".join(toc_lines))
            handle.write("\n")
    except Exception:
        Path(handle.name).unlink(missing_ok=True)
        raise
    return Path(handle.name)


def build_pg_restore_list_command(dump_path: Path) -> list[str]:
    return ["pg_restore", "--list", str(dump_path)]


def filter_agentic_restore_toc(toc_text: str) -> list[str]:
    selected_lines: list[str] = []

    for line in toc_text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith(";"):
            selected_lines.append(line)
            continue

        if _is_agentic_toc_entry(line):
            selected_lines.append(line)

    if not any(_is_agentic_toc_entry(line) for line in selected_lines):
        raise SnapshotCommandError(
            "snapshot dump does not contain any restoreable agentic schema entries"
        )

    return selected_lines


def validate_snapshot_artifact(dump_path: Path, manifest: dict[str, Any]) -> None:
    artifact = manifest["artifact"]
    expected_size = int(artifact["size_bytes"])
    actual_size = dump_path.stat().st_size
    if actual_size != expected_size:
        raise SnapshotCommandError(
            f"snapshot dump size mismatch: expected {expected_size} bytes, found {actual_size}"
        )

    expected_hash = artifact["content_hash"]
    actual_hash = compute_file_sha256(dump_path)
    if actual_hash != expected_hash:
        raise SnapshotCommandError(
            f"snapshot dump content hash mismatch: expected {expected_hash}, found {actual_hash}"
        )


def inspect_snapshot_import_target(database_url: str) -> SnapshotImportTargetInspection:
    psycopg = _load_psycopg()
    try:
        with psycopg.connect(database_url) as connection:
            with connection.cursor() as cursor:
                cursor.execute("SELECT to_regnamespace(%s) IS NOT NULL", (AGENTIC_SCHEMA,))
                schema_row = cursor.fetchone()
                schema_exists = bool(schema_row[0]) if schema_row else False
                if not schema_exists:
                    return SnapshotImportTargetInspection(
                        schema_exists=False,
                        state_table_row_counts={},
                    )

                row_counts: dict[str, int] = {}
                missing_tables: list[str] = []
                for table_name in SNAPSHOT_IMPORT_STATE_TABLES:
                    cursor.execute("SELECT to_regclass(%s)", (table_name,))
                    table_row = cursor.fetchone()
                    if not table_row or table_row[0] is None:
                        missing_tables.append(table_name)
                        continue

                    cursor.execute(f"SELECT COUNT(*) FROM {table_name}")
                    count_row = cursor.fetchone()
                    row_counts[table_name] = int(count_row[0] if count_row else 0)
    except SnapshotCommandError:
        raise
    except Exception as error:
        raise SnapshotCommandError(f"failed to inspect snapshot import target: {error}") from error

    if missing_tables:
        missing_summary = ", ".join(sorted(missing_tables))
        raise SnapshotCommandError(
            "snapshot import target does not match the expected KB schema state; "
            f"missing state tables: {missing_summary}. Recreate the disposable KB volume and retry import."
        )

    return SnapshotImportTargetInspection(
        schema_exists=True,
        state_table_row_counts=row_counts,
    )


def ensure_disposable_import_target(database_url: str) -> SnapshotImportTargetInspection:
    inspection = inspect_snapshot_import_target(database_url)
    if not inspection.schema_exists:
        return inspection

    if inspection.nonempty_tables:
        table_summary = ", ".join(
            f"{table_name}={row_count}"
            for table_name, row_count in sorted(inspection.nonempty_tables.items())
        )
        raise SnapshotCommandError(
            "snapshot import requires a fresh, isolated, or otherwise disposable KB database; "
            f"found existing rows in {table_summary}. Recreate the disposable KB volume and retry import."
        )

    return inspection


def compute_file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(SNAPSHOT_HASH_CHUNK_SIZE)
            if not chunk:
                break
            digest.update(chunk)
    return f"sha256:{digest.hexdigest()}"


def _is_agentic_toc_entry(line: str) -> bool:
    _, _, description = line.partition(";")
    tokens = description.strip().split()
    if len(tokens) < 4:
        return False

    type_token, schema_index = _toc_type_and_schema_index(tokens)
    if type_token == "SCHEMA":
        return len(tokens) > schema_index and tokens[schema_index] == AGENTIC_SCHEMA

    return len(tokens) > schema_index and tokens[schema_index] == AGENTIC_SCHEMA


def _toc_type_and_schema_index(tokens: list[str]) -> tuple[str, int]:
    if len(tokens) >= 6 and tokens[2] == "SCHEMA" and tokens[3] == "-":
        return "SCHEMA", 4
    if len(tokens) >= 5 and tokens[2] == "TABLE" and tokens[3] == "DATA":
        return "TABLE DATA", 4
    if len(tokens) >= 5 and tokens[2] == "FK" and tokens[3] == "CONSTRAINT":
        return "FK CONSTRAINT", 4
    if len(tokens) >= 5 and tokens[2] == "SEQUENCE" and tokens[3] == "SET":
        return "SEQUENCE SET", 4
    return tokens[2], 3


def _collect_export_metadata(
    connection: Any,
    cursor: Any,
    *,
    output_path: Path,
) -> SnapshotExportMetadata:
    now = _utcnow()
    sync_records = PostgresSyncStateStore(connection).list_sync_states()
    repo_name = _resolve_repo_name(sync_records)
    sync_state = normalize_sync_state_records(sync_records, repo=repo_name)
    entity_counts = _fetch_entity_counts(cursor)
    runtime_config = AgenticConfig.from_env()
    embedding_contract = build_runtime_embedding_contract(runtime_config)

    return SnapshotExportMetadata(
        snapshot_name=snapshot_name_from_dump_path(output_path),
        snapshot_created_at=now,
        repo_name=repo_name,
        docs_commit_hash=sync_state["docs"]["repo_commit_hash"],
        code_commit_hash=sync_state["code"]["repo_commit_hash"],
        embedding_contract=embedding_contract,
        entity_counts=entity_counts,
        sync_state=sync_state,
    )


def ensure_compatible_snapshot_import(manifest: dict[str, Any]) -> None:
    try:
        snapshot_contract = extract_snapshot_embedding_contract(manifest)
        runtime_contract = build_runtime_embedding_contract(AgenticConfig.from_env())
    except ValueError as error:
        raise SnapshotCommandError(
            f"{error}. Recreate the disposable KB volume and import a compatible snapshot or rebuild locally."
        ) from error

    mismatches = describe_embedding_contract_mismatches(snapshot_contract, runtime_contract)
    if not mismatches:
        return

    mismatch_detail = "; ".join(mismatches)
    raise SnapshotCommandError(
        "snapshot embedding contract is incompatible with this KB tooling: "
        f"{mismatch_detail}. Recreate the disposable KB volume and import a compatible snapshot or rebuild locally."
    )


def _resolve_repo_name(sync_records: list[SyncStateRecord]) -> str:
    for record in sync_records:
        repo_name = record.metadata.get("repo")
        if isinstance(repo_name, str) and repo_name.strip():
            return repo_name.strip()
        repo_name = _repo_name_from_scope_key(record.scope_key)
        if repo_name is not None:
            return repo_name
    return DEFAULT_SYNC_REPO


def _repo_name_from_scope_key(scope_key: str) -> str | None:
    if not scope_key.startswith("repo:"):
        return None
    remainder = scope_key[len("repo:") :]
    if not remainder:
        return None
    if remainder.count("/") >= 1 and ":" in remainder:
        repo_name, _, maybe_stream = remainder.rpartition(":")
        if maybe_stream in {"issues", "pulls", "issue_comments", "review_comments"}:
            return repo_name
    return remainder


def _fetch_entity_counts(cursor: Any) -> dict[str, int]:
    counts: dict[str, int] = {}
    for key, table_name in SNAPSHOT_ENTITY_TABLES:
        cursor.execute(f"SELECT COUNT(*) FROM {table_name}")
        row = cursor.fetchone()
        counts[key] = int(row[0] if row else 0)
    return {key: counts[key] for key in SNAPSHOT_ENTITY_COUNT_KEYS}


def _persist_snapshot_manifest(
    database_url: str,
    manifest: dict[str, Any] | None,
    *,
    source_path: str,
    imported_at: datetime | None = None,
) -> None:
    if manifest is None:
        raise SnapshotCommandError("snapshot manifest persistence requires a validated manifest")

    record = snapshot_manifest_record_fields(manifest, source_path=source_path, imported_at=imported_at)
    psycopg, Json = _load_psycopg(with_json=True)
    try:
        with psycopg.connect(database_url) as connection:
            with connection.transaction():
                with connection.cursor() as cursor:
                    cursor.execute(
                        """
                        INSERT INTO agentic.kb_snapshot_manifest (
                            id,
                            snapshot_name,
                            schema_version,
                            snapshot_created_at,
                            repo_commit_hash,
                            embedding_model,
                            entity_counts,
                            github_watermarks,
                            manifest,
                            source_path,
                            content_hash,
                            imported_at,
                            updated_at
                        ) VALUES (
                            %(id)s,
                            %(snapshot_name)s,
                            %(schema_version)s,
                            %(snapshot_created_at)s,
                            %(repo_commit_hash)s,
                            %(embedding_model)s,
                            %(entity_counts)s,
                            %(github_watermarks)s,
                            %(manifest)s,
                            %(source_path)s,
                            %(content_hash)s,
                            %(imported_at)s,
                            NOW()
                        )
                        ON CONFLICT (snapshot_name, snapshot_created_at) DO UPDATE SET
                            id = EXCLUDED.id,
                            schema_version = EXCLUDED.schema_version,
                            repo_commit_hash = EXCLUDED.repo_commit_hash,
                            embedding_model = EXCLUDED.embedding_model,
                            entity_counts = EXCLUDED.entity_counts,
                            github_watermarks = EXCLUDED.github_watermarks,
                            manifest = EXCLUDED.manifest,
                            source_path = EXCLUDED.source_path,
                            content_hash = EXCLUDED.content_hash,
                            imported_at = EXCLUDED.imported_at,
                            updated_at = NOW()
                        """,
                        {
                            **record,
                            "entity_counts": Json(record["entity_counts"]),
                            "github_watermarks": Json(record["github_watermarks"]),
                            "manifest": Json(record["manifest"]),
                        },
                    )
    except Exception as error:
        raise SnapshotCommandError(f"failed to persist snapshot manifest metadata: {error}") from error


def _load_manifest_file(path: Path) -> dict[str, Any]:
    try:
        with path.open("r", encoding="utf-8") as handle:
            manifest = json.load(handle)
    except FileNotFoundError:
        raise SnapshotCommandError(f"snapshot manifest does not exist: {path}") from None
    except json.JSONDecodeError as error:
        raise SnapshotCommandError(f"snapshot manifest is not valid JSON: {path}: {error.msg}") from error

    if not isinstance(manifest, dict):
        raise SnapshotCommandError(f"snapshot manifest must be a JSON object: {path}")
    try:
        validate_snapshot_manifest(manifest)
    except ValueError as error:
        raise SnapshotCommandError(str(error)) from error
    return manifest


def _write_manifest_file(path: Path, manifest: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        json.dump(manifest, handle, indent=2, sort_keys=False)
        handle.write("\n")


def _cleanup_partial_export(dump_path: Path, manifest_path: Path) -> None:
    dump_path.unlink(missing_ok=True)
    manifest_path.unlink(missing_ok=True)


def _default_snapshot_name(*, now: datetime | None = None) -> str:
    timestamp = (now or _utcnow()).astimezone(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return f"{DEFAULT_SNAPSHOT_PREFIX}-{timestamp}{SNAPSHOT_DUMP_SUFFIX}"


def _absolute_path(path: Path) -> Path:
    if path.is_absolute():
        return path
    return Path.cwd() / path


def _validated_endpoint(database_url: str):
    try:
        return parse_database_endpoint(database_url)
    except ValueError as error:
        raise SnapshotCommandError(str(error)) from error


def _database_url_from_env() -> str:
    database_url = (os.getenv("DATABASE_URL") or "").strip()
    if not database_url:
        raise SnapshotCommandError("DATABASE_URL is required for snapshot commands")
    _validated_endpoint(database_url)
    return database_url


def _ensure_required_binaries(*names: str) -> None:
    missing = [name for name in names if shutil.which(name) is None]
    if missing:
        raise SnapshotCommandError(
            "required PostgreSQL client tools are unavailable: " + ", ".join(missing)
        )


def _run_subprocess(command: list[str]) -> None:
    try:
        subprocess.run(command, check=True, capture_output=True, text=True)
    except subprocess.CalledProcessError as error:
        detail = (error.stderr or error.stdout or str(error)).strip()
        raise SnapshotCommandError(detail or f"command failed: {' '.join(command)}") from error


def _run_subprocess_capture(command: list[str]) -> str:
    try:
        completed = subprocess.run(command, check=True, capture_output=True, text=True)
    except subprocess.CalledProcessError as error:
        detail = (error.stderr or error.stdout or str(error)).strip()
        raise SnapshotCommandError(detail or f"command failed: {' '.join(command)}") from error
    return completed.stdout


def _utcnow() -> datetime:
    return datetime.now(timezone.utc)


def _load_psycopg(*, with_json: bool = False):
    import psycopg

    if not with_json:
        return psycopg

    from psycopg.types.json import Json

    return psycopg, Json
