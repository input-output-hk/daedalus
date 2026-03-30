from __future__ import annotations

import os
import shutil
import socket
import subprocess
import time
import unittest
import uuid
from pathlib import Path


COMPOSE_FILE = "docker-compose.agentic.yml"
COMPOSE_SMOKE_ENV = "AGENTIC_RUN_COMPOSE_SMOKE"
REPO_ROOT = Path(__file__).resolve().parents[2]
UP_TIMEOUT_SECONDS = 15 * 60
READINESS_TIMEOUT_SECONDS = 10 * 60
READINESS_POLL_INTERVAL_SECONDS = 5
LOG_TAIL_LINES = "200"
DEFAULT_EXCLUDED_PORTS = {5445, 11434}


class ComposeBootstrapSmokeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        _require_compose_smoke_opt_in()
        _require_docker_compose()
        _require_docker_daemon()

    def setUp(self) -> None:
        self.project_name = f"task709-smoke-{uuid.uuid4().hex[:10]}"
        self.db_port = _reserve_host_port(excluded_ports=DEFAULT_EXCLUDED_PORTS)
        self.ollama_port = _reserve_host_port(
            excluded_ports=DEFAULT_EXCLUDED_PORTS | {self.db_port}
        )
        self.compose_env = {
            **os.environ,
            "AGENTIC_DB_PORT": str(self.db_port),
            "OLLAMA_PORT": str(self.ollama_port),
        }
        self.addCleanup(self._tear_down_project)

    def test_repo_root_compose_stack_reaches_lightweight_healthcheck(self):
        up_result = _run_compose(
            self.project_name,
            ["up", "-d"],
            env=self.compose_env,
            timeout=UP_TIMEOUT_SECONDS,
        )
        if up_result.returncode != 0:
            self._fail_with_compose_context(
                "`docker compose up -d` failed",
                command_result=up_result,
            )

        db_port_result = _run_compose(
            self.project_name,
            ["port", "paradedb", "5432"],
            env=self.compose_env,
        )
        ollama_port_result = _run_compose(
            self.project_name,
            ["port", "ollama", "11434"],
            env=self.compose_env,
        )
        self.assertEqual(db_port_result.returncode, 0, db_port_result.stderr)
        self.assertEqual(ollama_port_result.returncode, 0, ollama_port_result.stderr)
        self.assertIn(f":{self.db_port}", db_port_result.stdout.strip())
        self.assertIn(f":{self.ollama_port}", ollama_port_result.stdout.strip())

        deadline = time.monotonic() + READINESS_TIMEOUT_SECONDS
        last_probe_result = None
        while time.monotonic() < deadline:
            probe_result = _run_compose(
                self.project_name,
                ["run", "--rm", "--no-deps", "kb-tools", "status", "--healthcheck"],
                env=self.compose_env,
                timeout=120,
            )
            if probe_result.returncode == 0:
                return
            last_probe_result = probe_result
            time.sleep(READINESS_POLL_INTERVAL_SECONDS)

        self._fail_with_compose_context(
            f"kb-tools readiness probe did not succeed within {READINESS_TIMEOUT_SECONDS} seconds",
            command_result=last_probe_result,
        )

    def _tear_down_project(self) -> None:
        down_result = _run_compose(
            self.project_name,
            ["down", "-v"],
            env=self.compose_env,
            timeout=300,
        )
        if down_result.returncode != 0:
            self._fail_with_compose_context(
                "`docker compose down -v` failed",
                command_result=down_result,
            )

    def _fail_with_compose_context(
        self,
        summary: str,
        *,
        command_result: subprocess.CompletedProcess[str] | None = None,
    ) -> None:
        parts = [summary]
        if command_result is not None:
            parts.append(_format_command_result(command_result))

        ps_result = _run_compose(
            self.project_name,
            ["ps"],
            env=self.compose_env,
            timeout=60,
        )
        logs_result = _run_compose(
            self.project_name,
            ["logs", "--no-color", "--tail", LOG_TAIL_LINES],
            env=self.compose_env,
            timeout=120,
        )
        parts.append("compose ps:\n" + _format_command_result(ps_result))
        parts.append("compose logs:\n" + _format_command_result(logs_result))
        self.fail("\n\n".join(parts))


def _require_compose_smoke_opt_in() -> None:
    if os.getenv(COMPOSE_SMOKE_ENV) != "1":
        raise unittest.SkipTest(f"requires {COMPOSE_SMOKE_ENV}=1")


def _require_docker_compose() -> None:
    if shutil.which("docker") is None:
        raise unittest.SkipTest("requires docker CLI")
    try:
        result = subprocess.run(
            ["docker", "compose", "version"],
            cwd=REPO_ROOT,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired) as error:
        raise unittest.SkipTest(f"docker compose is unavailable: {error}") from error
    if result.returncode != 0:
        raise unittest.SkipTest(
            "docker compose is unavailable:\n" + _format_command_result(result)
        )


def _require_docker_daemon() -> None:
    try:
        result = subprocess.run(
            ["docker", "info", "--format", "{{.ServerVersion}}"],
            cwd=REPO_ROOT,
            check=False,
            capture_output=True,
            text=True,
            timeout=30,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired) as error:
        raise unittest.SkipTest(f"Docker daemon is unavailable: {error}") from error
    if result.returncode != 0:
        raise unittest.SkipTest(
            "Docker daemon is unavailable:\n" + _format_command_result(result)
        )


def _run_compose(
    project_name: str,
    compose_args: list[str],
    *,
    env: dict[str, str],
    timeout: int = 120,
) -> subprocess.CompletedProcess[str]:
    command = [
        "docker",
        "compose",
        "-p",
        project_name,
        "-f",
        COMPOSE_FILE,
        *compose_args,
    ]
    try:
        return subprocess.run(
            command,
            cwd=REPO_ROOT,
            env=env,
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as error:
        return subprocess.CompletedProcess(
            command,
            returncode=124,
            stdout=_normalize_timeout_stream(error.stdout),
            stderr=_normalize_timeout_stream(error.stderr)
            + f"\nTimed out after {timeout} seconds.",
        )


def _format_command_result(result: subprocess.CompletedProcess[str]) -> str:
    stdout = result.stdout.strip() or "<empty>"
    stderr = result.stderr.strip() or "<empty>"
    command = " ".join(str(part) for part in result.args)
    return (
        f"command: {command}\n"
        f"exit_code: {result.returncode}\n"
        f"stdout:\n{stdout}\n"
        f"stderr:\n{stderr}"
    )


def _normalize_timeout_stream(stream: str | bytes | None) -> str:
    if stream is None:
        return ""
    if isinstance(stream, bytes):
        return stream.decode("utf-8", errors="replace")
    return stream


def _reserve_host_port(*, excluded_ports: set[int]) -> int:
    while True:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.bind(("127.0.0.1", 0))
            port = int(sock.getsockname()[1])
        if port not in excluded_ports:
            return port


if __name__ == "__main__":
    unittest.main()
