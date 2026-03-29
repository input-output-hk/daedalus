from __future__ import annotations

from collections import OrderedDict
from dataclasses import dataclass
from typing import Any, Mapping

from agentic_kb.commands.output import format_text_value, print_json, print_stderr
from agentic_kb.config import AgenticConfig
from agentic_kb.search import DEFAULT_SEARCH_LIMIT, PostgresSearchStore, SearchRequest, SearchValidationError


@dataclass(frozen=True)
class ParsedFilter:
    key: str
    value: Any


def add_search_arguments(parser) -> None:
    parser.add_argument("query", help="Search query text")
    parser.add_argument(
        "--mode",
        choices=("bm25", "vector", "hybrid"),
        default="hybrid",
        help="Search mode to execute (default: hybrid)",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=DEFAULT_SEARCH_LIMIT,
        help=f"Maximum number of results to return (default: {DEFAULT_SEARCH_LIMIT})",
    )
    parser.add_argument(
        "--entity-type",
        action="append",
        default=[],
        dest="entity_types",
        help="Restrict search to a registry entity type; repeat to include more than one",
    )
    parser.add_argument(
        "--filter",
        action="append",
        default=[],
        dest="filters",
        help="Apply a registry-backed filter as key=value; repeat for multiple filters",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit one JSON object to stdout on success",
    )


def run_search(args) -> int:
    try:
        request = build_search_request_from_args(args)
        config = AgenticConfig.from_env()
        with PostgresSearchStore.from_config(config) as store:
            result = store.search(request)
    except SearchValidationError as error:
        print_stderr(f"search failed: {error}")
        return 2
    except Exception as error:  # pragma: no cover - exercised by CLI/runtime tests
        print_stderr(f"search failed: {error}")
        return 1

    if getattr(args, "json", False):
        print_json(serialize_search_result_set(result))
    else:
        print_search_result_text(result)
    return 0


def build_search_request_from_args(args) -> SearchRequest:
    filters = parse_cli_filters(getattr(args, "entity_types", ()), getattr(args, "filters", ()))
    return SearchRequest(
        query_text=args.query,
        limit=args.limit,
        mode=args.mode,
        filters=filters,
    )


def parse_cli_filters(entity_types: list[str] | tuple[str, ...], raw_filters: list[str] | tuple[str, ...]) -> dict[str, Any]:
    filters: OrderedDict[str, Any] = OrderedDict()
    if entity_types:
        filters["entity_type"] = list(dict.fromkeys(entity_types))

    for raw_filter in raw_filters:
        parsed = parse_filter_argument(raw_filter)
        if parsed.key in filters:
            raise SearchValidationError(f"Duplicate search filter key: {parsed.key}")
        filters[parsed.key] = parsed.value

    return dict(filters)


def parse_filter_argument(raw_filter: str) -> ParsedFilter:
    if "=" not in raw_filter:
        raise SearchValidationError(
            f"Search filters must use key=value syntax: {raw_filter!r}"
        )
    key, value = raw_filter.split("=", 1)
    normalized_key = key.strip()
    normalized_value = value.strip()
    if not normalized_key:
        raise SearchValidationError(f"Search filters must define a key: {raw_filter!r}")
    if normalized_key == "entity_type":
        raise SearchValidationError("Use --entity-type instead of --filter entity_type=...")
    if not normalized_value:
        raise SearchValidationError(
            f"Search filter {normalized_key!r} must not be empty"
        )
    return ParsedFilter(key=normalized_key, value=normalized_value)


def serialize_search_result_set(result) -> dict[str, Any]:
    return {
        "query_text": result.query_text,
        "mode": result.mode,
        "limit": result.limit,
        "filters": dict(result.filters),
        "entity_types": list(result.entity_types),
        "hits": [serialize_search_hit(hit) for hit in result.hits],
    }


def serialize_search_hit(hit) -> dict[str, Any]:
    return {
        "entity_type": hit.entity_type,
        "source_domain": hit.source_domain,
        "id": hit.id,
        "fields": dict(hit.fields),
        "bm25_score": hit.bm25_score,
        "vector_distance": hit.vector_distance,
        "fused_score": hit.fused_score,
        "bm25_rank": hit.bm25_rank,
        "vector_rank": hit.vector_rank,
        "hybrid_rank": hit.hybrid_rank,
    }


def print_search_result_text(result) -> None:
    print(
        f"search {result.mode.value}: {len(result.hits)} hit(s) for {result.query_text!r}"
    )
    for index, hit in enumerate(result.hits, start=1):
        title = _best_display_title(hit.fields)
        location = _best_display_location(hit.fields)
        preview = _best_display_preview(hit.fields)
        print(f"{index}. {hit.entity_type.value} {hit.id} - {title}")
        if location:
            print(f"   location: {location}")
        if preview:
            print(f"   preview: {preview}")


def _best_display_title(fields: Mapping[str, Any]) -> str:
    for key in ("title", "symbol_name", "project_item_node_id", "issue_number", "pr_number"):
        value = fields.get(key)
        if value not in (None, ""):
            return format_text_value(value)
    return format_text_value(fields.get("id", "(untitled)"))


def _best_display_location(fields: Mapping[str, Any]) -> str | None:
    for key in ("source_path", "repo_path", "html_url"):
        value = fields.get(key)
        if value not in (None, ""):
            return format_text_value(value)
    return None


def _best_display_preview(fields: Mapping[str, Any]) -> str | None:
    for key in ("preview_text", "body_text", "content"):
        value = fields.get(key)
        if value not in (None, ""):
            text = format_text_value(value).replace("\n", " ").strip()
            return text[:240]
    return None
