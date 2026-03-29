from __future__ import annotations

from argparse import Namespace
from typing import Any, Mapping

from agentic_kb.commands.output import format_text_value, print_json, print_stderr
from agentic_kb.config import AgenticConfig
from agentic_kb.search import SearchValidationError, get_search_entity_config


ENTITY_NOT_FOUND_EXIT_CODE = 4
ENTITY_INVALID_TYPE_EXIT_CODE = 2


class EntityCommandError(RuntimeError):
    pass


def add_entity_subcommands(parser) -> None:
    subparsers = parser.add_subparsers(dest="entity_command", required=True)

    get_parser = subparsers.add_parser("get", help="Fetch one indexed entity by type and stable id")
    get_parser.add_argument("entity_type", help="Registry-backed entity type name")
    get_parser.add_argument("id", help="Stable row id to fetch")
    get_parser.add_argument(
        "--json",
        action="store_true",
        help="Emit one JSON object to stdout on success",
    )
    get_parser.set_defaults(handler=run_entity_get)


def run_entity_get(args) -> int:
    try:
        entity = get_entity_from_args(args)
    except SearchValidationError as error:
        print_stderr(f"entity get failed: {error}")
        return ENTITY_INVALID_TYPE_EXIT_CODE
    except EntityCommandError as error:
        print_stderr(f"entity get failed: {error}")
        return ENTITY_NOT_FOUND_EXIT_CODE
    except Exception as error:  # pragma: no cover - runtime path
        print_stderr(f"entity get failed: {error}")
        return 1

    if getattr(args, "json", False):
        print_json(serialize_entity_payload(entity))
    else:
        print_entity_text(entity)
    return 0


def get_entity_from_args(args) -> dict[str, Any]:
    return get_entity_payload(args.entity_type, args.id)


def get_entity_payload(entity_type: str, entity_id: str) -> dict[str, Any]:
    try:
        entity_config = get_search_entity_config(entity_type)
    except ValueError as error:
        raise SearchValidationError(f"Unsupported entity_type value: {entity_type!r}") from error

    config = AgenticConfig.from_env()
    if not config.database_url:
        raise SearchValidationError("DATABASE_URL is required for entity lookup")

    row = fetch_entity_row(config.database_url, entity_config.table_name, entity_config.primary_key_column, entity_id)
    if row is None:
        raise EntityCommandError(
            f"{entity_config.entity_type.value} entity not found for id {entity_id!r}"
        )
    return {
        "entity_type": entity_config.entity_type,
        "id": str(row[entity_config.primary_key_column]),
        "table_name": entity_config.table_name,
        "row": row,
    }


def build_entity_args(entity_type: str, entity_id: str) -> Namespace:
    return Namespace(entity_type=entity_type, id=entity_id, json=False)


def fetch_entity_row(
    database_url: str,
    table_name: str,
    primary_key_column: str,
    entity_id: str,
) -> dict[str, Any] | None:
    psycopg = _load_psycopg()
    schema_name, bare_table_name = table_name.split(".", 1)
    query = psycopg.sql.SQL("SELECT * FROM {}.{} WHERE {} = %s LIMIT 1").format(
        psycopg.sql.Identifier(schema_name),
        psycopg.sql.Identifier(bare_table_name),
        psycopg.sql.Identifier(primary_key_column),
    )

    with psycopg.connect(database_url) as connection:
        with connection.cursor(row_factory=psycopg.rows.dict_row) as cursor:
            cursor.execute(query, (entity_id,))
            row = cursor.fetchone()
    return dict(row) if row is not None else None


def serialize_entity_payload(entity: Mapping[str, Any]) -> dict[str, Any]:
    return {
        "entity_type": entity["entity_type"],
        "id": entity["id"],
        "table_name": entity["table_name"],
        "row": dict(entity["row"]),
    }


def print_entity_text(entity: Mapping[str, Any]) -> None:
    print(f"entity: {format_text_value(entity['entity_type'])} {entity['id']}")
    print(f"table: {entity['table_name']}")
    row = entity["row"]
    for key in sorted(row.keys()):
        print(f"{key}: {format_text_value(row[key])}")


def _load_psycopg():
    import psycopg

    return psycopg
