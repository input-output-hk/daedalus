from __future__ import annotations

import signal
import threading

from agentic_kb.commands.status import collect_status_report, print_status_report
from agentic_kb.config import AgenticConfig


def run_service(_args) -> int:
    config = AgenticConfig.from_env()
    report = collect_status_report(config, healthcheck=True)
    print_status_report(report)

    if not report.ok:
        print("service bootstrap failed; fix the reported task-103 readiness issues and restart kb-tools")
        return 1

    stop_event = threading.Event()

    def _stop(signum, _frame) -> None:
        print(f"received signal {signum}; stopping kb-tools service mode")
        stop_event.set()

    signal.signal(signal.SIGTERM, _stop)
    signal.signal(signal.SIGINT, _stop)

    print("kb-tools service mode is running; schema, sync, and snapshot work remain future tasks")

    while not stop_event.wait(timeout=1.0):
        continue

    return 0
