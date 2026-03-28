from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

from agentic_kb.config import parse_database_endpoint


DEFAULT_SNAPSHOTS_PATH = Path("/workspace/agentic/snapshots")
DEFAULT_SNAPSHOT_PREFIX = "agentic-kb"
AGENTIC_SCHEMA = "agentic"


@dataclass(frozen=True)
class SnapshotResult:
    path: Path
    database_name: str


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
    import_parser.add_argument("path", help="Path to a previously exported snapshot dump")
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

    print(f"snapshot export complete: {result.path} ({result.database_name})")
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
        source_path = resolve_import_path(args.path)
        result = import_snapshot(database_url, source_path, confirmed=True)
    except SnapshotCommandError as error:
        print(f"snapshot import failed: {error}")
        return 2

    print(f"snapshot import complete: {result.path} ({result.database_name})")
    return 0


def export_snapshot(database_url: str, destination: str | Path) -> SnapshotResult:
    endpoint = _validated_endpoint(database_url)
    output_path = resolve_export_path(destination)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    _ensure_required_binaries("pg_dump")
    _run_subprocess(build_pg_dump_command(database_url, output_path))
    return SnapshotResult(path=output_path, database_name=endpoint.database)


def import_snapshot(
    database_url: str,
    source_path: str | Path,
    *,
    confirmed: bool,
) -> SnapshotResult:
    if not confirmed:
        raise SnapshotCommandError("snapshot import requires explicit destructive confirmation")

    endpoint = _validated_endpoint(database_url)
    dump_path = resolve_import_path(source_path)
    _ensure_required_binaries("psql", "pg_restore")
    restore_list_path = build_agentic_restore_list(dump_path)
    _run_subprocess(build_drop_schema_command(database_url))
    try:
        _run_subprocess(build_pg_restore_command(database_url, dump_path, restore_list_path))
    finally:
        restore_list_path.unlink(missing_ok=True)
    return SnapshotResult(path=dump_path, database_name=endpoint.database)


def resolve_export_path(path_value: str | Path | None, *, now: datetime | None = None) -> Path:
    if path_value is None:
        return DEFAULT_SNAPSHOTS_PATH / _default_snapshot_name(now=now)

    path = Path(path_value).expanduser()
    if path.exists() and path.is_dir():
        return path / _default_snapshot_name(now=now)
    return _absolute_path(path)


def resolve_import_path(path_value: str | Path) -> Path:
    path = _absolute_path(Path(path_value).expanduser())
    if not path.exists():
        raise SnapshotCommandError(f"snapshot dump does not exist: {path}")
    if not path.is_file():
        raise SnapshotCommandError(f"snapshot dump is not a file: {path}")
    return path


def build_pg_dump_command(database_url: str, output_path: Path) -> list[str]:
    return [
        "pg_dump",
        "--format=custom",
        "--compress=6",
        "--no-owner",
        "--no-privileges",
        f"--schema={AGENTIC_SCHEMA}",
        f"--file={output_path}",
        database_url,
    ]


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


def _default_snapshot_name(*, now: datetime | None = None) -> str:
    timestamp = (now or _utcnow()).astimezone(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return f"{DEFAULT_SNAPSHOT_PREFIX}-{timestamp}.dump"


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
