from __future__ import annotations

import argparse

from agentic_kb.commands.entity import add_entity_subcommands
from agentic_kb.commands.output import print_stderr
from agentic_kb.commands.search import add_search_arguments, run_search
from agentic_kb.commands.service import run_service
from agentic_kb.commands.snapshot import add_snapshot_subcommands
from agentic_kb.commands.status import run_status
from agentic_kb.commands.sync import add_sync_subcommands
from agentic_kb.mcp import run_mcp_search


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
    status_parser.add_argument(
        "--json",
        action="store_true",
        help="Emit one JSON object to stdout on success",
    )
    status_parser.set_defaults(handler=run_status)

    search_parser = subparsers.add_parser(
        "search",
        help="Run KB search queries against the configured database",
    )
    add_search_arguments(search_parser)
    search_parser.set_defaults(handler=run_search)

    entity_parser = subparsers.add_parser(
        "entity",
        help="Inspect one indexed KB entity",
    )
    add_entity_subcommands(entity_parser)

    sync_parser = subparsers.add_parser(
        "sync",
        help="Sync docs, code, GitHub, project, or incremental changes",
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

    mcp_search_parser = subparsers.add_parser(
        "mcp-search",
        help="Run the read-only Search MCP server over stdio",
    )
    mcp_search_parser.set_defaults(handler=run_mcp_search)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    handler = getattr(args, "handler", None)

    if handler is None:
        parser.print_help()
        return 1

    try:
        return int(handler(args) or 0)
    except BrokenPipeError:
        return 0
    except Exception as error:  # pragma: no cover - top-level runtime guard
        print_stderr(str(error))
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
