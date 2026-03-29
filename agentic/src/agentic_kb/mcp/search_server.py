from __future__ import annotations

import json
import re
import sys
from dataclasses import dataclass
from typing import Any, BinaryIO, Callable, Mapping, Sequence

from agentic_kb import __version__
from agentic_kb.commands.entity import EntityCommandError, get_entity_payload, serialize_entity_payload
from agentic_kb.commands.output import json_safe_value
from agentic_kb.commands.search import serialize_search_hit, serialize_search_result_set
from agentic_kb.commands.status import collect_status_report, serialize_status_report
from agentic_kb.config import AgenticConfig
from agentic_kb.search import (
    DEFAULT_SEARCH_LIMIT,
    PostgresSearchStore,
    SearchEntityType,
    SearchMode,
    SearchRequest,
    SearchValidationError,
    get_search_entity_config,
    list_search_entity_configs,
)

JSONRPC_VERSION = "2.0"
DEFAULT_PROTOCOL_VERSION = "2025-03-26"
RELATED_QUERY_MAX_CHARS = 4000
RELATED_DEFAULT_LIMIT = 5
RELATED_MIN_OVERFETCH = 20
RELATED_MAX_OVERFETCH = 50
MCP_INVALID_REQUEST = -32600
MCP_METHOD_NOT_FOUND = -32601
MCP_INVALID_PARAMS = -32602
MCP_INTERNAL_ERROR = -32603
MCP_TOOL_NOT_FOUND = -32001
MCP_ENTITY_NOT_FOUND = -32004
WHITESPACE_RE = re.compile(r"\s+")

ALL_ENTITY_TYPES = tuple(config.entity_type for config in list_search_entity_configs())
ALL_FILTER_KEYS = tuple(
    sorted(
        {
            filter_config.key
            for config in list_search_entity_configs()
            for filter_config in config.filters
        }
    )
)
DOC_FILTER_KEYS = ("doc_kind", "source_path_prefix")
CODE_FILTER_KEYS = ("repo_path_prefix", "language", "symbol_kind")
GITHUB_FILTER_KEYS = ("repo", "state", "comment_type")
GITHUB_ENTITY_TYPES = (
    SearchEntityType.GITHUB_ISSUES,
    SearchEntityType.GITHUB_ISSUE_COMMENTS,
    SearchEntityType.GITHUB_PRS,
    SearchEntityType.GITHUB_PR_COMMENTS,
)


@dataclass(frozen=True)
class McpToolDefinition:
    name: str
    description: str
    input_schema: dict[str, Any]
    handler: Callable[[Mapping[str, Any]], Mapping[str, Any]]


class McpRequestError(RuntimeError):
    def __init__(self, code: int, message: str, *, data: Mapping[str, Any] | None = None):
        super().__init__(message)
        self.code = code
        self.message = message
        self.data = dict(data) if data is not None else None


class SearchMcpServer:
    def __init__(self, config: AgenticConfig | None = None):
        self._config = config or AgenticConfig.from_env()
        self._tools = self._build_tools()

    def list_tools(self) -> list[dict[str, Any]]:
        return [
            {
                "name": tool.name,
                "description": tool.description,
                "inputSchema": tool.input_schema,
            }
            for tool in self._tools.values()
        ]

    def handle_message(self, message: Mapping[str, Any]) -> dict[str, Any] | None:
        request_id = message.get("id")
        try:
            method = self._require_string(message.get("method"), field_name="method")
            params = message.get("params", {})
            if params is None:
                params = {}
            if not isinstance(params, Mapping):
                raise McpRequestError(MCP_INVALID_PARAMS, "params must be an object")
            result = self._dispatch(method=method, params=params)
        except McpRequestError as error:
            if request_id is None:
                return None
            return self._error_response(request_id, error)
        except Exception as error:  # pragma: no cover - exercised through stdio smoke tests
            if request_id is None:
                return None
            return self._error_response(
                request_id,
                McpRequestError(MCP_INTERNAL_ERROR, str(error)),
            )

        if request_id is None:
            return None
        return {"jsonrpc": JSONRPC_VERSION, "id": request_id, "result": result}

    def call_tool(self, name: str, arguments: Mapping[str, Any] | None = None) -> dict[str, Any]:
        tool = self._tools.get(name)
        if tool is None:
            raise McpRequestError(MCP_TOOL_NOT_FOUND, f"Unknown tool: {name}")

        if arguments is None:
            arguments = {}
        if not isinstance(arguments, Mapping):
            raise McpRequestError(MCP_INVALID_PARAMS, "tool arguments must be an object")

        try:
            payload = tool.handler(arguments)
        except SearchValidationError as error:
            raise McpRequestError(MCP_INVALID_PARAMS, str(error)) from error
        except EntityCommandError as error:
            raise McpRequestError(MCP_ENTITY_NOT_FOUND, str(error)) from error

        safe_payload = json_safe_value(payload)
        serialized_payload = json.dumps(safe_payload, ensure_ascii=True, separators=(",", ":"))
        return {
            "structuredContent": safe_payload,
            "content": [{"type": "text", "text": serialized_payload}],
        }

    def _dispatch(self, *, method: str, params: Mapping[str, Any]) -> Mapping[str, Any]:
        if method == "initialize":
            return self._handle_initialize(params)
        if method == "ping":
            return {}
        if method == "tools/list":
            return {"tools": self.list_tools()}
        if method == "tools/call":
            name = self._require_string(params.get("name"), field_name="name")
            arguments = params.get("arguments", {})
            if arguments is None:
                arguments = {}
            return self.call_tool(name, arguments)
        if method == "notifications/initialized":
            return {}
        raise McpRequestError(MCP_METHOD_NOT_FOUND, f"Method not found: {method}")

    def _handle_initialize(self, params: Mapping[str, Any]) -> dict[str, Any]:
        requested_version = params.get("protocolVersion")
        protocol_version = (
            requested_version
            if isinstance(requested_version, str) and requested_version.strip()
            else DEFAULT_PROTOCOL_VERSION
        )
        return {
            "protocolVersion": protocol_version,
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "agentic-kb-search", "version": __version__},
        }

    def _build_tools(self) -> dict[str, McpToolDefinition]:
        tool_definitions = (
            McpToolDefinition(
                name="search",
                description="Search the knowledge base across all supported entity types.",
                input_schema=_search_input_schema(include_entity_types=True, filter_keys=ALL_FILTER_KEYS),
                handler=self._handle_search,
            ),
            McpToolDefinition(
                name="search_docs",
                description="Search only indexed documentation rows.",
                input_schema=_search_input_schema(include_entity_types=False, filter_keys=DOC_FILTER_KEYS),
                handler=self._handle_search_docs,
            ),
            McpToolDefinition(
                name="search_code",
                description="Search only indexed code chunks.",
                input_schema=_search_input_schema(include_entity_types=False, filter_keys=CODE_FILTER_KEYS),
                handler=self._handle_search_code,
            ),
            McpToolDefinition(
                name="search_github",
                description="Search indexed GitHub issues, pull requests, and comments.",
                input_schema=_search_input_schema(include_entity_types=True, filter_keys=GITHUB_FILTER_KEYS),
                handler=self._handle_search_github,
            ),
            McpToolDefinition(
                name="get_entity",
                description="Fetch one indexed entity row by type and stable id.",
                input_schema={
                    "type": "object",
                    "properties": {
                        "entity_type": {"type": "string"},
                        "id": {"type": "string"},
                    },
                    "required": ["entity_type", "id"],
                    "additionalProperties": False,
                },
                handler=self._handle_get_entity,
            ),
            McpToolDefinition(
                name="find_related",
                description="Run a BM25-only related-results search derived from an indexed entity.",
                input_schema={
                    "type": "object",
                    "properties": {
                        "entity_type": {"type": "string"},
                        "id": {"type": "string"},
                        "limit": {"type": "integer", "minimum": 1},
                    },
                    "required": ["entity_type", "id"],
                    "additionalProperties": False,
                },
                handler=self._handle_find_related,
            ),
            McpToolDefinition(
                name="kb_status",
                description="Return the packaged KB runtime and database status payload.",
                input_schema={
                    "type": "object",
                    "properties": {},
                    "additionalProperties": False,
                },
                handler=self._handle_kb_status,
            ),
        )
        return {tool.name: tool for tool in tool_definitions}

    def _handle_search(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        request = _build_search_request(
            arguments,
            allowed_filter_keys=ALL_FILTER_KEYS,
            default_entity_types=None,
            allow_entity_types=True,
        )
        return self._execute_search(request)

    def _handle_search_docs(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        request = _build_search_request(
            arguments,
            allowed_filter_keys=DOC_FILTER_KEYS,
            default_entity_types=(SearchEntityType.DOCUMENTS,),
            allow_entity_types=False,
        )
        return self._execute_search(request)

    def _handle_search_code(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        request = _build_search_request(
            arguments,
            allowed_filter_keys=CODE_FILTER_KEYS,
            default_entity_types=(SearchEntityType.CODE_CHUNKS,),
            allow_entity_types=False,
        )
        return self._execute_search(request)

    def _handle_search_github(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        explicit_entity_types = _parse_entity_types(
            arguments,
            allowed_entity_types=GITHUB_ENTITY_TYPES,
            allow_entity_types=True,
        )
        filters = _parse_filters(arguments, allowed_filter_keys=GITHUB_FILTER_KEYS)
        resolved_entity_types = _resolve_github_entity_types(explicit_entity_types, filters)
        if explicit_entity_types is not None:
            _validate_entity_type_filter_compatibility(explicit_entity_types, filters.keys())
        request = _build_search_request(
            arguments,
            allowed_filter_keys=GITHUB_FILTER_KEYS,
            default_entity_types=resolved_entity_types,
            allow_entity_types=True,
            override_entity_types=resolved_entity_types,
        )
        return self._execute_search(request)

    def _handle_get_entity(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        _validate_argument_keys(arguments, allowed_keys=("entity_type", "id"))
        entity_type = self._require_string(arguments.get("entity_type"), field_name="entity_type")
        entity_id = self._require_string(arguments.get("id"), field_name="id")
        payload = get_entity_payload(entity_type, entity_id)
        return serialize_entity_payload(payload)

    def _handle_find_related(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        _validate_argument_keys(arguments, allowed_keys=("entity_type", "id", "limit"))
        entity_type = self._require_string(arguments.get("entity_type"), field_name="entity_type")
        entity_id = self._require_string(arguments.get("id"), field_name="id")
        limit = _get_positive_int(arguments, key="limit", default=RELATED_DEFAULT_LIMIT)
        entity_payload = get_entity_payload(entity_type, entity_id)
        entity_config = get_search_entity_config(entity_type)
        query_text = _derive_related_query_text(entity_config.text_columns, entity_payload["row"])
        request = SearchRequest(
            query_text=query_text,
            mode=SearchMode.BM25,
            limit=_related_overfetch_limit(limit),
            filters={},
        )
        with PostgresSearchStore.from_config(self._config) as store:
            result = store.search(request)
        filtered_hits = [
            hit
            for hit in result.hits
            if not (hit.entity_type.value == entity_type and hit.id == entity_id)
        ][:limit]
        return {
            "seed_entity_type": entity_type,
            "seed_id": entity_id,
            "query_text": query_text,
            "mode": SearchMode.BM25,
            "limit": limit,
            "hits": [serialize_search_hit(hit) for hit in filtered_hits],
        }

    def _handle_kb_status(self, arguments: Mapping[str, Any]) -> dict[str, Any]:
        _validate_argument_keys(arguments, allowed_keys=())
        report = collect_status_report(self._config, healthcheck=False)
        return serialize_status_report(report)

    def _execute_search(self, request: SearchRequest) -> dict[str, Any]:
        with PostgresSearchStore.from_config(self._config) as store:
            result = store.search(request)
        return serialize_search_result_set(result)

    def _error_response(self, request_id: Any, error: McpRequestError) -> dict[str, Any]:
        payload: dict[str, Any] = {
            "jsonrpc": JSONRPC_VERSION,
            "id": request_id,
            "error": {
                "code": error.code,
                "message": error.message,
            },
        }
        if error.data is not None:
            payload["error"]["data"] = json_safe_value(error.data)
        return payload

    @staticmethod
    def _require_string(value: Any, *, field_name: str) -> str:
        if not isinstance(value, str):
            raise McpRequestError(MCP_INVALID_PARAMS, f"{field_name} must be a string")
        return value


def run_stdio_server(
    *,
    input_stream: BinaryIO | None = None,
    output_stream: BinaryIO | None = None,
    config: AgenticConfig | None = None,
) -> int:
    server = SearchMcpServer(config=config)
    stdin = input_stream or sys.stdin.buffer
    stdout = output_stream or sys.stdout.buffer

    while True:
        message = _read_message(stdin)
        if message is None:
            return 0
        response = server.handle_message(message)
        if response is not None and not (
            message.get("method") == "notifications/initialized" and message.get("id") is None
        ):
            _write_message(stdout, response)


def run_mcp_search(_args) -> int:
    return run_stdio_server()


def _build_search_request(
    arguments: Mapping[str, Any],
    *,
    allowed_filter_keys: Sequence[str],
    default_entity_types: Sequence[SearchEntityType] | None,
    allow_entity_types: bool,
    override_entity_types: Sequence[SearchEntityType] | None = None,
) -> SearchRequest:
    allowed_keys = ["query_text", "mode", "limit", "filters"]
    if allow_entity_types:
        allowed_keys.append("entity_types")
    _validate_argument_keys(arguments, allowed_keys=tuple(allowed_keys))

    query_text = arguments.get("query_text")
    if not isinstance(query_text, str):
        raise SearchValidationError("query_text must be a string")

    mode = arguments.get("mode", SearchMode.HYBRID.value)
    limit = _get_positive_int(arguments, key="limit", default=DEFAULT_SEARCH_LIMIT)
    filters = _parse_filters(arguments, allowed_filter_keys=allowed_filter_keys)
    entity_types = override_entity_types
    if entity_types is None:
        entity_types = _parse_entity_types(
            arguments,
            allowed_entity_types=ALL_ENTITY_TYPES,
            allow_entity_types=allow_entity_types,
        )
    if entity_types is None:
        entity_types = default_entity_types

    request_filters = dict(filters)
    if entity_types is not None:
        request_filters["entity_type"] = [entity_type.value for entity_type in entity_types]

    return SearchRequest(
        query_text=query_text,
        mode=mode,
        limit=limit,
        filters=request_filters,
    )


def _parse_filters(arguments: Mapping[str, Any], *, allowed_filter_keys: Sequence[str]) -> dict[str, str]:
    raw_filters = arguments.get("filters", {})
    if raw_filters is None:
        return {}
    if not isinstance(raw_filters, Mapping):
        raise SearchValidationError("filters must be an object")

    filters: dict[str, str] = {}
    allowed = set(allowed_filter_keys)
    for raw_key, raw_value in raw_filters.items():
        if not isinstance(raw_key, str):
            raise SearchValidationError("filters must use string keys")
        if raw_key not in allowed:
            raise SearchValidationError(f"Unsupported search filters: {raw_key}")
        if not isinstance(raw_value, str):
            raise SearchValidationError(f"Filter {raw_key!r} must be a string")
        filters[raw_key] = raw_value
    return filters


def _parse_entity_types(
    arguments: Mapping[str, Any],
    *,
    allowed_entity_types: Sequence[SearchEntityType],
    allow_entity_types: bool,
) -> tuple[SearchEntityType, ...] | None:
    raw_entity_types = arguments.get("entity_types")
    if raw_entity_types is None:
        return None
    if not allow_entity_types:
        raise SearchValidationError("entity_types is not supported for this tool")
    if not isinstance(raw_entity_types, list):
        raise SearchValidationError("entity_types must be an array of strings")

    normalized: list[SearchEntityType] = []
    allowed = set(allowed_entity_types)
    for raw_value in raw_entity_types:
        try:
            entity_type = SearchEntityType(raw_value)
        except ValueError as error:
            raise SearchValidationError(f"Unsupported entity_type value: {raw_value!r}") from error
        if entity_type not in allowed:
            raise SearchValidationError(f"Unsupported entity_type value: {raw_value!r}")
        if entity_type not in normalized:
            normalized.append(entity_type)

    return tuple(normalized)


def _validate_entity_type_filter_compatibility(
    entity_types: Sequence[SearchEntityType],
    filter_keys: Sequence[str],
) -> None:
    for entity_type in entity_types:
        config = get_search_entity_config(entity_type)
        supported_keys = {filter_config.key for filter_config in config.filters}
        if any(filter_key not in supported_keys for filter_key in filter_keys):
            raise SearchValidationError("Selected entity_type values do not support the requested filters")


def _resolve_github_entity_types(
    explicit_entity_types: Sequence[SearchEntityType] | None,
    filters: Mapping[str, str],
) -> tuple[SearchEntityType, ...]:
    resolved = tuple(explicit_entity_types) if explicit_entity_types is not None else GITHUB_ENTITY_TYPES
    if "state" in filters:
        resolved = tuple(
            entity_type
            for entity_type in resolved
            if entity_type in (SearchEntityType.GITHUB_ISSUES, SearchEntityType.GITHUB_PRS)
        )
    if "comment_type" in filters:
        resolved = tuple(
            entity_type
            for entity_type in resolved
            if entity_type == SearchEntityType.GITHUB_PR_COMMENTS
        )
    if not resolved:
        raise SearchValidationError("Selected entity_type values do not support the requested filters")
    return resolved


def _derive_related_query_text(text_columns: Sequence[str], row: Mapping[str, Any]) -> str:
    parts: list[str] = []
    for column_name in text_columns:
        normalized = _normalize_related_text(row.get(column_name))
        if normalized:
            parts.append(normalized)
    query_text = " ".join(parts).strip()[:RELATED_QUERY_MAX_CHARS].strip()
    if not query_text:
        raise SearchValidationError("find_related requires non-empty seed text from configured text_columns")
    return query_text


def _normalize_related_text(value: Any) -> str:
    if value in (None, ""):
        return ""
    normalized = json_safe_value(value)
    if not isinstance(normalized, str):
        normalized = json.dumps(normalized, ensure_ascii=True, separators=(",", ":"))
    return WHITESPACE_RE.sub(" ", normalized).strip()


def _related_overfetch_limit(limit: int) -> int:
    return min(max(limit * 3, RELATED_MIN_OVERFETCH), RELATED_MAX_OVERFETCH)


def _get_positive_int(arguments: Mapping[str, Any], *, key: str, default: int) -> int:
    value = arguments.get(key, default)
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise SearchValidationError(f"{key} must be a positive integer")
    return value


def _validate_argument_keys(arguments: Mapping[str, Any], *, allowed_keys: Sequence[str]) -> None:
    allowed = set(allowed_keys)
    extras = sorted(str(key) for key in arguments.keys() if key not in allowed)
    if extras:
        raise SearchValidationError(f"Unexpected tool arguments: {', '.join(extras)}")


def _search_input_schema(*, include_entity_types: bool, filter_keys: Sequence[str]) -> dict[str, Any]:
    properties: dict[str, Any] = {
        "query_text": {"type": "string"},
        "mode": {"type": "string", "enum": [mode.value for mode in SearchMode]},
        "limit": {"type": "integer", "minimum": 1},
        "filters": {
            "type": "object",
            "properties": {key: {"type": "string"} for key in filter_keys},
            "additionalProperties": False,
        },
    }
    required = ["query_text"]
    if include_entity_types:
        properties["entity_types"] = {
            "type": "array",
            "items": {"type": "string"},
            "uniqueItems": True,
        }
    return {
        "type": "object",
        "properties": properties,
        "required": required,
        "additionalProperties": False,
    }


def _read_message(stream: BinaryIO) -> dict[str, Any] | None:
    headers: dict[str, str] = {}
    while True:
        line = stream.readline()
        if line == b"":
            return None
        if line in (b"\r\n", b"\n"):
            break
        if b":" not in line:
            raise McpRequestError(MCP_INVALID_REQUEST, "Malformed MCP header")
        key, value = line.decode("utf-8").split(":", 1)
        headers[key.strip().lower()] = value.strip()

    content_length = headers.get("content-length")
    if content_length is None:
        raise McpRequestError(MCP_INVALID_REQUEST, "Missing Content-Length header")

    try:
        body_size = int(content_length)
    except ValueError as error:
        raise McpRequestError(MCP_INVALID_REQUEST, "Invalid Content-Length header") from error

    body = stream.read(body_size)
    if len(body) != body_size:
        return None
    try:
        payload = json.loads(body.decode("utf-8"))
    except json.JSONDecodeError as error:
        raise McpRequestError(MCP_INVALID_REQUEST, f"Invalid JSON request body: {error}") from error
    if not isinstance(payload, dict):
        raise McpRequestError(MCP_INVALID_REQUEST, "JSON-RPC payload must be an object")
    return payload


def _write_message(stream: BinaryIO, payload: Mapping[str, Any]) -> None:
    encoded = json.dumps(payload, ensure_ascii=True, separators=(",", ":")).encode("utf-8")
    stream.write(f"Content-Length: {len(encoded)}\r\n\r\n".encode("ascii"))
    stream.write(encoded)
    stream.flush()
