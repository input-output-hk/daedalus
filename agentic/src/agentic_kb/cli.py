from __future__ import annotations

import argparse

from agentic_kb.commands.service import run_service
from agentic_kb.commands.snapshot import add_snapshot_subcommands
from agentic_kb.commands.status import run_status
from agentic_kb.commands.sync import add_sync_subcommands


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="agentic-kb",
        description="Daedalus agentic knowledge-base CLI",
    )
    subparsers = parser.add_subparsers(dest="command")

    status_parser = subparsers.add_parser(
        "status",
        help="Show runtime, dependency, and KB database readiness",
    )
    status_parser.add_argument(
        "--healthcheck",
        action="store_true",
        help="Exit non-zero when required runtime config or dependencies are unavailable",
    )
    status_parser.set_defaults(handler=run_status)

    sync_parser = subparsers.add_parser(
        "sync",
        help="Grouped sync commands reserved for later tasks",
    )
    add_sync_subcommands(sync_parser)

    snapshot_parser = subparsers.add_parser(
        "snapshot",
        help="Export and import KB database snapshots",
    )
    add_snapshot_subcommands(snapshot_parser)

    service_parser = subparsers.add_parser(
        "service",
        help="Run long-lived service mode for docker compose",
    )
    service_parser.set_defaults(handler=run_service)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    handler = getattr(args, "handler", None)

    if handler is None:
        parser.print_help()
        return 1

    return int(handler(args) or 0)


if __name__ == "__main__":
    raise SystemExit(main())
