from __future__ import annotations


SNAPSHOT_TASK_GUIDANCE = {
    "export": "task-602 will add real snapshot export after the DB status and manifest work from task-205 and task-601.",
    "import": "task-602 will add real snapshot import after the DB status and manifest work from task-205 and task-601.",
}


def add_snapshot_subcommands(parser) -> None:
    subparsers = parser.add_subparsers(dest="snapshot_command", required=True)

    export_parser = subparsers.add_parser("export", help="Placeholder for snapshot export")
    export_parser.set_defaults(handler=_run_snapshot_placeholder, placeholder_name="export")

    import_parser = subparsers.add_parser("import", help="Placeholder for snapshot import")
    import_parser.add_argument("path", nargs="?", help="Future snapshot path argument")
    import_parser.set_defaults(handler=_run_snapshot_placeholder, placeholder_name="import")


def _run_snapshot_placeholder(args) -> int:
    name = args.placeholder_name
    print(
        f"snapshot {name} is not implemented in task-103. "
        f"Use `agentic-kb status` for bootstrap checks today; {SNAPSHOT_TASK_GUIDANCE[name]}"
    )
    return 2
