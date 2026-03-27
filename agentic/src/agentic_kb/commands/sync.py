from __future__ import annotations


SYNC_TASK_GUIDANCE = {
    "all": "task-701 will implement the real sync orchestration after schema and ingestion foundations land.",
    "changed": "task-701 will implement delta sync after sync-state tracking from task-405 exists.",
    "docs": "task-701 will implement docs sync on top of the docs ingestion work from task-301.",
    "code": "task-701 will implement code sync on top of the code ingestion work from task-401 and task-402.",
    "github": "task-701 will implement GitHub sync after task-403 is available.",
    "project": "task-701 will implement project sync after task-404 is available.",
}


def add_sync_subcommands(parser) -> None:
    subparsers = parser.add_subparsers(dest="sync_command", required=True)

    for name in ("all", "changed", "docs", "code", "github", "project"):
        command_parser = subparsers.add_parser(name, help=f"Placeholder for sync {name}")
        command_parser.set_defaults(handler=_run_sync_placeholder, placeholder_name=name)


def _run_sync_placeholder(args) -> int:
    name = args.placeholder_name
    print(
        f"sync {name} is not implemented in task-103. "
        f"Use `agentic-kb status` for bootstrap checks today; {SYNC_TASK_GUIDANCE[name]}"
    )
    return 2
