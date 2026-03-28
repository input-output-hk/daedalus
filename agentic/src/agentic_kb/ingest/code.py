from __future__ import annotations

import hashlib
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path, PurePosixPath
from typing import Any, Protocol, Sequence

from tree_sitter import Node
from tree_sitter_language_pack import get_parser

from agentic_kb.config import AgenticConfig
from agentic_kb.embed import OllamaEmbeddingClient
from agentic_kb.ingest.docs import (
    _embed_document_content,
    _load_psycopg,
    _normalize_content,
    _vector_literal,
    get_repo_commit_hash,
    normalize_source_path,
)


CODE_SOURCE_PATTERNS: tuple[str, ...] = (
    "source/common/**/*.ts",
    "source/main/**/*.ts",
    "source/renderer/app/**/*.ts",
    "source/renderer/app/**/*.tsx",
)
CODE_PREVIEW_LENGTH = 280
SUPPORTED_DECLARATION_TYPES = {
    "class_declaration",
    "function_declaration",
    "interface_declaration",
    "type_alias_declaration",
    "enum_declaration",
    "lexical_declaration",
}
FUNCTION_LIKE_MEMBER_VALUE_TYPES = {
    "arrow_function",
    "function_expression",
    "generator_function",
}


class EmbeddingClient(Protocol):
    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        ...


class CodeChunksStore(Protocol):
    def replace_chunks_for_path(
        self, repo_path: str, chunks: Sequence["PreparedCodeChunk"]
    ) -> int:
        ...


@dataclass(frozen=True)
class PreparedCodeChunk:
    id: str
    repo_path: str
    language: str
    symbol_name: str | None
    symbol_kind: str | None
    parent_symbol_name: str | None
    parent_symbol_kind: str | None
    chunk_index: int
    start_line: int
    end_line: int
    content: str
    preview_text: str
    content_hash: str
    repo_commit_hash: str
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class CodeIngestResult:
    source_paths: tuple[str, ...]
    processed_file_count: int
    chunk_count: int
    repo_commit_hash: str


@dataclass(frozen=True)
class ExtractedCodeSymbol:
    symbol_name: str | None
    symbol_kind: str | None
    parent_symbol_name: str | None
    parent_symbol_kind: str | None
    start_line: int
    end_line: int
    content: str
    metadata: dict[str, Any]
    sort_key: tuple[int, int, int]


@dataclass
class _ExportBinding:
    export_name: str | None
    is_default_export: bool
    exported_via_list: bool


@dataclass
class _DeclarationCandidate:
    local_name: str | None
    symbol_kind: str
    symbol_node: Node
    content_node: Node
    exports: list[_ExportBinding] = field(default_factory=list)


def discover_code_source_paths(workspace_root: str | Path) -> list[str]:
    root = Path(workspace_root).resolve()
    matches: set[str] = set()

    for pattern in CODE_SOURCE_PATTERNS:
        for matched_path in root.glob(pattern):
            if not matched_path.is_file():
                continue
            repo_path = normalize_source_path(root, matched_path)
            if is_supported_code_path(repo_path):
                matches.add(repo_path)

    return sorted(matches)


def is_supported_code_path(repo_path: str) -> bool:
    path = PurePosixPath(repo_path)
    suffixes = path.suffixes
    if suffixes[-2:] == [".d", ".ts"]:
        return False
    if suffixes[-3:] == [".scss", ".d", ".ts"]:
        return False
    return path.suffix in {".ts", ".tsx"}


def classify_code_language(repo_path: str) -> str:
    return "typescriptreact" if repo_path.endswith(".tsx") else "typescript"


def parse_codebase_source_paths(
    workspace_root: str | Path,
    *,
    source_paths: Sequence[str] | None = None,
) -> tuple[str, ...]:
    root = Path(workspace_root).resolve()
    resolved_source_paths = _resolve_source_paths(root, source_paths)

    for repo_path in resolved_source_paths:
        content = _read_source_content(root, repo_path)
        parse_typescript_source(
            repo_path,
            content,
            language=classify_code_language(repo_path),
        )

    return tuple(resolved_source_paths)


def extract_typescript_symbol_chunks(
    repo_path: str,
    content: str,
    *,
    language: str | None = None,
) -> list[ExtractedCodeSymbol]:
    normalized_content = _normalize_content(content)
    active_language = language or classify_code_language(repo_path)
    tree = parse_typescript_source(repo_path, normalized_content, language=active_language)
    source_bytes = normalized_content.encode("utf-8")
    declarations = _collect_declarations(tree.root_node, source_bytes)

    extracted: list[ExtractedCodeSymbol] = []
    for declaration in sorted(declarations, key=lambda item: item.symbol_node.start_byte):
        export_bindings = _dedupe_export_bindings(declaration.exports)
        if not export_bindings:
            continue

        for export_order, binding in enumerate(export_bindings):
            symbol_name = binding.export_name or declaration.local_name or "default"
            member_symbols = (
                _extract_class_member_symbols(
                    declaration.symbol_node,
                    source_bytes,
                    parent_symbol_name=symbol_name,
                )
                if declaration.symbol_kind == "class"
                else []
            )
            metadata = {
                "symbol_path": symbol_name,
                "is_default_export": binding.is_default_export,
                "exported_via_list": binding.exported_via_list,
                "has_member_chunks": bool(member_symbols),
            }
            if declaration.local_name and declaration.local_name != symbol_name:
                metadata["local_symbol_name"] = declaration.local_name

            extracted.append(
                _build_extracted_symbol(
                    node=declaration.content_node,
                    source_bytes=source_bytes,
                    symbol_name=symbol_name,
                    symbol_kind=declaration.symbol_kind,
                    parent_symbol_name=None,
                    parent_symbol_kind=None,
                    metadata=metadata,
                    sort_key=(declaration.symbol_node.start_byte, 0, export_order),
                )
            )

            for member_order, member in enumerate(member_symbols, start=1):
                extracted.append(
                    ExtractedCodeSymbol(
                        symbol_name=member.symbol_name,
                        symbol_kind=member.symbol_kind,
                        parent_symbol_name=member.parent_symbol_name,
                        parent_symbol_kind=member.parent_symbol_kind,
                        start_line=member.start_line,
                        end_line=member.end_line,
                        content=member.content,
                        metadata=member.metadata,
                        sort_key=(member.sort_key[0], member.sort_key[1], export_order * 1000 + member_order),
                    )
                )

    return sorted(extracted, key=lambda symbol: symbol.sort_key)


def prepare_code_chunks(
    workspace_root: str | Path,
    *,
    source_paths: Sequence[str] | None = None,
    embedding_client: EmbeddingClient,
    repo_commit_hash: str | None = None,
) -> list[PreparedCodeChunk]:
    root = Path(workspace_root).resolve()
    resolved_source_paths = _resolve_source_paths(root, source_paths)
    resolved_repo_commit_hash = repo_commit_hash or get_repo_commit_hash(root)
    prepared_chunks: list[PreparedCodeChunk] = []

    for repo_path in resolved_source_paths:
        prepared_chunks.extend(
            _prepare_file_code_chunks(
                root,
                repo_path,
                embedding_client=embedding_client,
                repo_commit_hash=resolved_repo_commit_hash,
            )
        )

    return prepared_chunks


def ingest_code(
    workspace_root: str | Path,
    *,
    embedding_client: EmbeddingClient,
    code_store: CodeChunksStore,
    source_paths: Sequence[str] | None = None,
    repo_commit_hash: str | None = None,
) -> CodeIngestResult:
    root = Path(workspace_root).resolve()
    resolved_source_paths = _resolve_source_paths(root, source_paths)
    resolved_repo_commit_hash = repo_commit_hash or get_repo_commit_hash(root)
    chunk_count = 0

    for repo_path in resolved_source_paths:
        prepared_chunks = _prepare_file_code_chunks(
            root,
            repo_path,
            embedding_client=embedding_client,
            repo_commit_hash=resolved_repo_commit_hash,
        )
        code_store.replace_chunks_for_path(repo_path, prepared_chunks)
        chunk_count += len(prepared_chunks)

    return CodeIngestResult(
        source_paths=tuple(resolved_source_paths),
        processed_file_count=len(resolved_source_paths),
        chunk_count=chunk_count,
        repo_commit_hash=resolved_repo_commit_hash,
    )


def ingest_code_from_config(
    workspace_root: str | Path,
    *,
    config: AgenticConfig | None = None,
    source_paths: Sequence[str] | None = None,
    repo_commit_hash: str | None = None,
) -> CodeIngestResult:
    active_config = config or AgenticConfig.from_env()
    if not active_config.database_url:
        raise ValueError("DATABASE_URL is required for code ingestion")

    embedding_client = OllamaEmbeddingClient.from_config(active_config)
    with PostgresCodeChunksStore.from_database_url(active_config.database_url) as code_store:
        return ingest_code(
            workspace_root,
            embedding_client=embedding_client,
            code_store=code_store,
            source_paths=source_paths,
            repo_commit_hash=repo_commit_hash,
        )


def parse_typescript_source(repo_path: str, content: str, *, language: str) -> Any:
    parser = _get_parser(language)
    tree = parser.parse(content.encode("utf-8"))
    if tree.root_node.has_error:
        error_node = _find_error_node(tree.root_node)
        location = "unknown"
        if error_node is not None:
            location = f"line {error_node.start_point.row + 1}, column {error_node.start_point.column + 1}"
        raise ValueError(f"Failed to parse {repo_path} with {language} parser at {location}")
    return tree


def deterministic_code_chunk_id(repo_path: str, *, chunk_index: int) -> str:
    return f"code:{repo_path}#{chunk_index}"


def deterministic_code_content_hash(
    repo_path: str,
    *,
    symbol_name: str | None,
    symbol_kind: str | None,
    parent_symbol_name: str | None,
    parent_symbol_kind: str | None,
    content: str,
) -> str:
    digest = hashlib.sha256()
    for value in (
        repo_path,
        symbol_name or "",
        symbol_kind or "",
        parent_symbol_name or "",
        parent_symbol_kind or "",
        content,
    ):
        digest.update(value.encode("utf-8"))
        digest.update(b"\0")
    return digest.hexdigest()


def build_code_preview_text(content: str, *, max_length: int = CODE_PREVIEW_LENGTH) -> str:
    normalized = " ".join(line.strip() for line in content.splitlines() if line.strip())
    if not normalized:
        raise ValueError("code chunks must not be empty or whitespace-only")
    if len(normalized) <= max_length:
        return normalized
    return normalized[: max_length - 3].rstrip() + "..."


def _resolve_source_paths(
    workspace_root: Path,
    source_paths: Sequence[str] | None,
) -> list[str]:
    candidates = source_paths or discover_code_source_paths(workspace_root)
    resolved = []
    for source_path in candidates:
        repo_path = normalize_source_path(
            workspace_root,
            workspace_root / PurePosixPath(source_path),
        )
        if not is_supported_code_path(repo_path):
            continue
        resolved.append(repo_path)
    return resolved


def _prepare_file_code_chunks(
    workspace_root: Path,
    repo_path: str,
    *,
    embedding_client: EmbeddingClient,
    repo_commit_hash: str,
) -> list[PreparedCodeChunk]:
    content = _read_source_content(workspace_root, repo_path)
    extracted_symbols = extract_typescript_symbol_chunks(repo_path, content)
    if not extracted_symbols:
        return []

    embeddings = [
        _embed_document_content(symbol.content, embedding_client=embedding_client)
        for symbol in extracted_symbols
    ]
    language = classify_code_language(repo_path)
    prepared_chunks: list[PreparedCodeChunk] = []

    for chunk_index, (symbol, embedding) in enumerate(
        zip(extracted_symbols, embeddings, strict=True)
    ):
        prepared_chunks.append(
            PreparedCodeChunk(
                id=deterministic_code_chunk_id(repo_path, chunk_index=chunk_index),
                repo_path=repo_path,
                language=language,
                symbol_name=symbol.symbol_name,
                symbol_kind=symbol.symbol_kind,
                parent_symbol_name=symbol.parent_symbol_name,
                parent_symbol_kind=symbol.parent_symbol_kind,
                chunk_index=chunk_index,
                start_line=symbol.start_line,
                end_line=symbol.end_line,
                content=symbol.content,
                preview_text=build_code_preview_text(symbol.content),
                content_hash=deterministic_code_content_hash(
                    repo_path,
                    symbol_name=symbol.symbol_name,
                    symbol_kind=symbol.symbol_kind,
                    parent_symbol_name=symbol.parent_symbol_name,
                    parent_symbol_kind=symbol.parent_symbol_kind,
                    content=symbol.content,
                ),
                repo_commit_hash=repo_commit_hash,
                embedding=embedding,
                metadata=dict(symbol.metadata),
            )
        )

    return prepared_chunks


def _collect_declarations(root_node: Node, source_bytes: bytes) -> list[_DeclarationCandidate]:
    declarations: list[_DeclarationCandidate] = []
    declarations_by_name: dict[str, list[_DeclarationCandidate]] = {}
    export_references: list[tuple[str, _ExportBinding]] = []

    for child in root_node.named_children:
        if child.type == "export_statement":
            declaration_node = child.child_by_field_name("declaration")
            if declaration_node is not None:
                direct_default_export = _has_child_type(child, "default")
                for declaration in _extract_declaration_candidates(
                    declaration_node,
                    content_node=child,
                ):
                    export_name = declaration.local_name or "default"
                    declaration.exports.append(
                        _ExportBinding(
                            export_name=export_name,
                            is_default_export=direct_default_export,
                            exported_via_list=False,
                        )
                    )
                    declarations.append(declaration)
                    _index_declaration(declarations_by_name, declaration)
                continue

            if child.child_by_field_name("source") is not None:
                continue

            if _has_child_type(child, "default"):
                value_node = child.child_by_field_name("value")
                if value_node is not None and value_node.type in {"identifier", "type_identifier"}:
                    export_references.append(
                        (
                            _node_text(value_node, source_bytes),
                            _ExportBinding(
                                export_name=_node_text(value_node, source_bytes),
                                is_default_export=True,
                                exported_via_list=False,
                            ),
                        )
                    )
                continue

            export_clause = _first_child_of_type(child, "export_clause")
            if export_clause is None:
                continue
            for specifier in export_clause.named_children:
                if specifier.type != "export_specifier":
                    continue
                local_name_node = specifier.child_by_field_name("name")
                if local_name_node is None:
                    continue
                alias_node = specifier.child_by_field_name("alias")
                local_name = _node_text(local_name_node, source_bytes)
                export_name = _node_text(alias_node, source_bytes) if alias_node else local_name
                export_references.append(
                    (
                        local_name,
                        _ExportBinding(
                            export_name=export_name,
                            is_default_export=False,
                            exported_via_list=True,
                        ),
                    )
                )
            continue

        if child.type in SUPPORTED_DECLARATION_TYPES:
            for declaration in _extract_declaration_candidates(child, content_node=child):
                declarations.append(declaration)
                _index_declaration(declarations_by_name, declaration)

    for local_name, export_binding in export_references:
        for declaration in declarations_by_name.get(local_name, []):
            declaration.exports.append(export_binding)

    return declarations


def _extract_declaration_candidates(
    declaration_node: Node,
    *,
    content_node: Node,
) -> list[_DeclarationCandidate]:
    if declaration_node.type == "lexical_declaration":
        candidates: list[_DeclarationCandidate] = []
        for child in declaration_node.named_children:
            if child.type != "variable_declarator":
                continue
            name_node = child.child_by_field_name("name")
            if name_node is None:
                continue
            binding_names = _extract_binding_names(name_node)
            for binding_name in binding_names:
                candidates.append(
                    _DeclarationCandidate(
                        local_name=binding_name,
                        symbol_kind="variable",
                        symbol_node=child,
                        content_node=content_node,
                    )
                )
        return candidates

    symbol_kind = {
        "class_declaration": "class",
        "function_declaration": "function",
        "interface_declaration": "interface",
        "type_alias_declaration": "type_alias",
        "enum_declaration": "enum",
    }.get(declaration_node.type)
    if symbol_kind is None:
        return []

    name_node = declaration_node.child_by_field_name("name")
    local_name = None
    if name_node is not None and name_node.type in {"identifier", "type_identifier", "property_identifier"}:
        local_name = name_node.text.decode("utf-8")

    return [
        _DeclarationCandidate(
            local_name=local_name,
            symbol_kind=symbol_kind,
            symbol_node=declaration_node,
            content_node=content_node,
        )
    ]


def _extract_binding_names(name_node: Node) -> list[str]:
    if name_node.type in {"identifier", "property_identifier"}:
        return [name_node.text.decode("utf-8")]

    if name_node.type in {"object_pattern", "array_pattern"}:
        names: list[str] = []
        for child in name_node.named_children:
            names.extend(_extract_binding_names(child))
        return names

    if name_node.type == "shorthand_property_identifier_pattern":
        return [name_node.text.decode("utf-8")]

    if name_node.type == "pair_pattern":
        value_node = name_node.child_by_field_name("value")
        return _extract_binding_names(value_node) if value_node is not None else []

    if name_node.type in {"assignment_pattern", "object_assignment_pattern"}:
        left_node = name_node.child_by_field_name("left")
        return _extract_binding_names(left_node) if left_node is not None else []

    if name_node.type == "rest_pattern":
        named_children = list(name_node.named_children)
        if not named_children:
            return []
        return _extract_binding_names(named_children[-1])

    return []


def _extract_class_member_symbols(
    class_node: Node,
    source_bytes: bytes,
    *,
    parent_symbol_name: str,
) -> list[ExtractedCodeSymbol]:
    class_body = class_node.child_by_field_name("body")
    if class_body is None:
        return []

    members: list[ExtractedCodeSymbol] = []
    for member in class_body.named_children:
        if member.type == "method_definition":
            member_name_node = member.child_by_field_name("name")
            if member_name_node is None or member_name_node.type not in {
                "property_identifier",
                "private_property_identifier",
                "identifier",
            }:
                continue
            member_name = _node_text(member_name_node, source_bytes)
            member_kind = _class_method_kind(member)
            members.append(
                _build_extracted_symbol(
                    node=member,
                    source_bytes=source_bytes,
                    symbol_name=member_name,
                    symbol_kind=member_kind,
                    parent_symbol_name=parent_symbol_name,
                    parent_symbol_kind="class",
                    metadata={
                        "symbol_path": f"{parent_symbol_name}.{member_name}",
                        "is_static": _has_child_type(member, "static"),
                    },
                    sort_key=(member.start_byte, 1, 0),
                )
            )
            continue

        if member.type != "public_field_definition":
            continue

        value_node = member.child_by_field_name("value")
        if value_node is None or value_node.type not in FUNCTION_LIKE_MEMBER_VALUE_TYPES:
            continue
        member_name_node = member.child_by_field_name("name")
        if member_name_node is None or member_name_node.type not in {
            "property_identifier",
            "private_property_identifier",
            "identifier",
        }:
            continue
        member_name = _node_text(member_name_node, source_bytes)
        members.append(
            _build_extracted_symbol(
                node=member,
                source_bytes=source_bytes,
                symbol_name=member_name,
                symbol_kind="field_function",
                parent_symbol_name=parent_symbol_name,
                parent_symbol_kind="class",
                metadata={
                    "symbol_path": f"{parent_symbol_name}.{member_name}",
                    "is_static": _has_child_type(member, "static"),
                },
                sort_key=(member.start_byte, 1, 0),
            )
        )

    return members


def _build_extracted_symbol(
    *,
    node: Node,
    source_bytes: bytes,
    symbol_name: str | None,
    symbol_kind: str | None,
    parent_symbol_name: str | None,
    parent_symbol_kind: str | None,
    metadata: dict[str, Any],
    sort_key: tuple[int, int, int],
) -> ExtractedCodeSymbol:
    return ExtractedCodeSymbol(
        symbol_name=symbol_name,
        symbol_kind=symbol_kind,
        parent_symbol_name=parent_symbol_name,
        parent_symbol_kind=parent_symbol_kind,
        start_line=node.start_point.row + 1,
        end_line=node.end_point.row + 1,
        content=_node_text(node, source_bytes),
        metadata=metadata,
        sort_key=sort_key,
    )


def _class_method_kind(member: Node) -> str:
    if _has_child_type(member, "get"):
        return "getter"
    if _has_child_type(member, "set"):
        return "setter"
    return "method"


def _index_declaration(
    declarations_by_name: dict[str, list[_DeclarationCandidate]],
    declaration: _DeclarationCandidate,
) -> None:
    if declaration.local_name is None:
        return
    declarations_by_name.setdefault(declaration.local_name, []).append(declaration)


def _dedupe_export_bindings(bindings: Sequence[_ExportBinding]) -> list[_ExportBinding]:
    seen: set[tuple[str | None, bool, bool]] = set()
    deduped: list[_ExportBinding] = []
    for binding in bindings:
        key = (binding.export_name, binding.is_default_export, binding.exported_via_list)
        if key in seen:
            continue
        seen.add(key)
        deduped.append(binding)
    return deduped


def _read_source_content(workspace_root: Path, repo_path: str) -> str:
    file_path = workspace_root / PurePosixPath(repo_path)
    return _normalize_content(file_path.read_text(encoding="utf-8"))


def _node_text(node: Node, source_bytes: bytes) -> str:
    return source_bytes[node.start_byte : node.end_byte].decode("utf-8")


def _find_error_node(node: Node) -> Node | None:
    if node.type == "ERROR":
        return node
    for child in node.children:
        found = _find_error_node(child)
        if found is not None:
            return found
    return None


def _has_child_type(node: Node, child_type: str) -> bool:
    return any(child.type == child_type for child in node.children)


def _first_child_of_type(node: Node, child_type: str) -> Node | None:
    for child in node.children:
        if child.type == child_type:
            return child
    return None


@lru_cache(maxsize=2)
def _get_parser(language: str) -> Any:
    parser_name = "tsx" if language == "typescriptreact" else "typescript"
    return get_parser(parser_name)


class PostgresCodeChunksStore:
    def __init__(self, connection: Any):
        self._connection = connection

    @classmethod
    def from_database_url(cls, database_url: str) -> "PostgresCodeChunksStore":
        psycopg, _ = _load_psycopg()
        return cls(psycopg.connect(database_url))

    def __enter__(self) -> "PostgresCodeChunksStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def replace_chunks_for_path(
        self, repo_path: str, chunks: Sequence[PreparedCodeChunk]
    ) -> int:
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.execute(
                    "DELETE FROM agentic.kb_code_chunks WHERE repo_path = %s",
                    (repo_path,),
                )
                if chunks:
                    cursor.executemany(
                        """
                        INSERT INTO agentic.kb_code_chunks (
                            id,
                            repo_path,
                            language,
                            symbol_name,
                            symbol_kind,
                            parent_symbol_name,
                            parent_symbol_kind,
                            chunk_index,
                            start_line,
                            end_line,
                            content,
                            preview_text,
                            content_hash,
                            repo_commit_hash,
                            embedding,
                            metadata,
                            updated_at
                        ) VALUES (
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s,
                            %s::vector,
                            %s,
                            NOW()
                        )
                        """,
                        [
                            (
                                chunk.id,
                                chunk.repo_path,
                                chunk.language,
                                chunk.symbol_name,
                                chunk.symbol_kind,
                                chunk.parent_symbol_name,
                                chunk.parent_symbol_kind,
                                chunk.chunk_index,
                                chunk.start_line,
                                chunk.end_line,
                                chunk.content,
                                chunk.preview_text,
                                chunk.content_hash,
                                chunk.repo_commit_hash,
                                _vector_literal(chunk.embedding),
                                Json(chunk.metadata),
                            )
                            for chunk in chunks
                        ],
                    )
        return len(chunks)


class InMemoryCodeChunksStore:
    def __init__(self):
        self.rows_by_key: dict[tuple[str, int], dict[str, Any]] = {}
        self.write_count = 0

    def replace_chunks_for_path(
        self, repo_path: str, chunks: Sequence[PreparedCodeChunk]
    ) -> int:
        existing_keys = [key for key in self.rows_by_key if key[0] == repo_path]
        for key in existing_keys:
            del self.rows_by_key[key]

        for chunk in chunks:
            self.write_count += 1
            key = (chunk.repo_path, chunk.chunk_index)
            self.rows_by_key[key] = {
                "id": chunk.id,
                "repo_path": chunk.repo_path,
                "language": chunk.language,
                "symbol_name": chunk.symbol_name,
                "symbol_kind": chunk.symbol_kind,
                "parent_symbol_name": chunk.parent_symbol_name,
                "parent_symbol_kind": chunk.parent_symbol_kind,
                "chunk_index": chunk.chunk_index,
                "start_line": chunk.start_line,
                "end_line": chunk.end_line,
                "content": chunk.content,
                "preview_text": chunk.preview_text,
                "content_hash": chunk.content_hash,
                "repo_commit_hash": chunk.repo_commit_hash,
                "embedding": list(chunk.embedding),
                "metadata": dict(chunk.metadata),
                "updated_at_token": self.write_count,
            }

        return len(chunks)
