from __future__ import annotations

import json
import socket
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable
from urllib.error import URLError
from urllib.parse import urljoin
from urllib.request import urlopen

from agentic_kb.config import AgenticConfig, parse_database_endpoint


WORKSPACE_PATH = "/workspace"
SNAPSHOTS_PATH = "/workspace/agentic/snapshots"


@dataclass(frozen=True)
class CheckResult:
    name: str
    ok: bool
    detail: str


@dataclass(frozen=True)
class StatusReport:
    healthcheck: bool
    ok: bool
    config_items: tuple[CheckResult, ...]
    environment_items: tuple[CheckResult, ...]
    dependency_items: tuple[CheckResult, ...]
    notes: tuple[str, ...]


def run_status(args) -> int:
    config = AgenticConfig.from_env()
    report = collect_status_report(config, healthcheck=bool(args.healthcheck))
    print_status_report(report)
    return 0 if report.ok or not args.healthcheck else 1


def collect_status_report(config: AgenticConfig, healthcheck: bool) -> StatusReport:
    config_items = list(_config_results(config, healthcheck=healthcheck))
    dependency_items = list(_dependency_results(config, healthcheck=healthcheck))
    environment_items = list(_environment_results())
    ok = all(item.ok for item in config_items + environment_items + dependency_items)
    notes = (
        "schema readiness is intentionally not checked in task-103",
        "sync and snapshot commands are placeholders until later tasks land",
    )

    return StatusReport(
        healthcheck=healthcheck,
        ok=ok,
        config_items=tuple(config_items),
        environment_items=tuple(environment_items),
        dependency_items=tuple(dependency_items),
        notes=notes,
    )


def print_status_report(report: StatusReport) -> None:
    mode = "healthcheck" if report.healthcheck else "status"
    state = "ok" if report.ok else "degraded"
    print(f"agentic-kb {mode}: {state}")

    _print_section("config", report.config_items)
    _print_section("environment", report.environment_items)
    _print_section("dependencies", report.dependency_items)

    print("notes:")
    for note in report.notes:
        print(f"- {note}")


def _print_section(title: str, items: Iterable[CheckResult]) -> None:
    print(f"{title}:")
    for item in items:
        marker = "ok" if item.ok else "warn"
        print(f"- [{marker}] {item.name}: {item.detail}")


def _config_results(config: AgenticConfig, healthcheck: bool) -> Iterable[CheckResult]:
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

    github_detail = "present" if config.github_token else "absent (optional in task-103)"
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
