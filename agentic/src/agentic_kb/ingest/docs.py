from __future__ import annotations

import hashlib
import re
import subprocess
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path, PurePosixPath
from typing import Any, Protocol, Sequence

from agentic_kb.config import AgenticConfig
from agentic_kb.embed import EmbeddingResponseError, OllamaEmbeddingClient
from agentic_kb.sync.state import (
    DEFAULT_SYNC_REPO,
    PostgresSyncStateStore,
    build_docs_sync_state,
    build_sync_attempt,
    build_sync_failure,
    repo_scope_key,
)


DOC_SOURCE_PATTERNS: tuple[str, ...] = (
    "AGENTS.md",
    "CLAUDE.md",
    "README.md",
    ".agent/readme.md",
    ".agent/workflows/**/*.md",
    ".agent/skills/**/*.md",
    ".agent/SOPs/**/*.md",
    ".agent/plans/**/*.md",
    "tests/README.md",
    "tests/news/README.md",
    "installers/README.md",
    "installers/icons/README.md",
    "source/renderer/app/themes/README.md",
)
DOC_PREVIEW_LENGTH = 280
EMBED_BATCH_MAX_TEXTS = 8
EMBED_BATCH_MAX_CHARS = 16000
EMBED_SEGMENT_TARGET_CHARS = 600
EMBED_SEGMENT_MIN_CHARS = 100
FIRST_H1_PATTERN = re.compile(r"^\s*#(?!#)\s+(.+?)\s*$", re.MULTILINE)


class EmbeddingClient(Protocol):
    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        ...


class DocsStore(Protocol):
    def upsert_documents(self, documents: Sequence["PreparedDocument"]) -> int:
        ...

    def delete_documents_for_paths(self, source_paths: Sequence[str]) -> int:
        ...

    def list_document_paths(self) -> list[str]:
        ...


@dataclass(frozen=True)
class PreparedDocument:
    id: str
    source_domain: str
    doc_kind: str
    source_path: str
    title: str
    section_title: str | None
    subsection_title: str | None
    heading_path: list[str]
    chunk_index: int
    content: str
    preview_text: str
    content_hash: str
    repo_commit_hash: str
    source_updated_at: datetime
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class DocsIngestResult:
    source_paths: tuple[str, ...]
    processed_count: int
    repo_commit_hash: str


def discover_docs_source_paths(workspace_root: str | Path) -> list[str]:
    root = Path(workspace_root).resolve()
    matches: set[str] = set()

    for pattern in DOC_SOURCE_PATTERNS:
        for matched_path in root.glob(pattern):
            if not matched_path.is_file():
                continue
            matches.add(normalize_source_path(root, matched_path))

    return sorted(matches)


def is_allowlisted_doc_path(source_path: str) -> bool:
    path = PurePosixPath(source_path)
    return any(path.match(pattern) for pattern in DOC_SOURCE_PATTERNS)


def prepare_documents(
    workspace_root: str | Path,
    *,
    source_paths: Sequence[str] | None = None,
    embedding_client: EmbeddingClient,
    repo_commit_hash: str | None = None,
) -> list[PreparedDocument]:
    root = Path(workspace_root).resolve()
    resolved_source_paths = [
        normalize_source_path(root, root / PurePosixPath(source_path))
        for source_path in (source_paths or discover_docs_source_paths(root))
    ]
    resolved_repo_commit_hash = repo_commit_hash or get_repo_commit_hash(root)

    document_payloads = [
        _load_document_payload(root, source_path, repo_commit_hash=resolved_repo_commit_hash)
        for source_path in resolved_source_paths
    ]
    embeddings = [
        _embed_document_content(payload["content"], embedding_client=embedding_client)
        for payload in document_payloads
    ]

    documents: list[PreparedDocument] = []
    for payload, embedding in zip(document_payloads, embeddings, strict=True):
        documents.append(
            PreparedDocument(
                id=payload["id"],
                source_domain="docs",
                doc_kind=payload["doc_kind"],
                source_path=payload["source_path"],
                title=payload["title"],
                section_title=None,
                subsection_title=None,
                heading_path=[],
                chunk_index=0,
                content=payload["content"],
                preview_text=payload["preview_text"],
                content_hash=payload["content_hash"],
                repo_commit_hash=payload["repo_commit_hash"],
                source_updated_at=payload["source_updated_at"],
                embedding=embedding,
                metadata=payload["metadata"],
            )
        )

    return documents


def ingest_docs(
    workspace_root: str | Path,
    *,
    embedding_client: EmbeddingClient,
    docs_store: DocsStore,
    repo_commit_hash: str | None = None,
) -> DocsIngestResult:
    documents = prepare_documents(
        workspace_root,
        embedding_client=embedding_client,
        repo_commit_hash=repo_commit_hash,
    )
    docs_store.upsert_documents(documents)

    return DocsIngestResult(
        source_paths=tuple(document.source_path for document in documents),
        processed_count=len(documents),
        repo_commit_hash=documents[0].repo_commit_hash if documents else (repo_commit_hash or get_repo_commit_hash(workspace_root)),
    )


def ingest_docs_from_config(
    workspace_root: str | Path,
    *,
    config: AgenticConfig | None = None,
    repo_commit_hash: str | None = None,
) -> DocsIngestResult:
    active_config = config or AgenticConfig.from_env()
    if not active_config.database_url:
        raise ValueError("DATABASE_URL is required for docs ingestion")

    embedding_client = OllamaEmbeddingClient.from_config(active_config)
    attempted_at = datetime.now(timezone.utc)
    scope_key = repo_scope_key(DEFAULT_SYNC_REPO)
    with PostgresDocsStore.from_database_url(active_config.database_url) as docs_store:
        with PostgresSyncStateStore.from_database_url(active_config.database_url) as sync_store:
            sync_store.record_attempts(
                [
                    build_sync_attempt(
                        "docs",
                        scope_key,
                        attempted_at=attempted_at,
                        metadata={"repo": DEFAULT_SYNC_REPO},
                    )
                ]
            )
            try:
                result = ingest_docs(
                    workspace_root,
                    embedding_client=embedding_client,
                    docs_store=docs_store,
                    repo_commit_hash=repo_commit_hash,
                )
            except Exception as error:
                sync_store.record_failures(
                    [
                        build_sync_failure(
                            "docs",
                            scope_key,
                            attempted_at=attempted_at,
                            error=error,
                            metadata={"repo": DEFAULT_SYNC_REPO},
                        )
                    ]
                )
                raise

            sync_store.upsert_sync_states(
                [
                    build_docs_sync_state(
                        result,
                        attempted_at=attempted_at,
                        succeeded_at=datetime.now(timezone.utc),
                        repo=DEFAULT_SYNC_REPO,
                    )
                ]
            )
            return result


def normalize_source_path(workspace_root: str | Path, file_path: str | Path) -> str:
    root = Path(workspace_root).resolve()
    candidate = Path(file_path).resolve()
    relative_path = candidate.relative_to(root)
    normalized = relative_path.as_posix()
    if normalized.startswith("./"):
        return normalized[2:]
    return normalized


def classify_doc_kind(source_path: str) -> str:
    if source_path == ".agent/readme.md":
        return "agent_index"
    if source_path in {"AGENTS.md", "CLAUDE.md"}:
        return "agent_instruction"
    if source_path.startswith(".agent/workflows/"):
        return "workflow"
    if source_path.startswith(".agent/skills/"):
        return "skill"
    if source_path.startswith(".agent/SOPs/"):
        return "sop"
    if source_path.startswith(".agent/plans/"):
        return "plan"
    return "readme"


def build_preview_text(content: str, *, max_length: int = DOC_PREVIEW_LENGTH) -> str:
    normalized = " ".join(line.strip() for line in content.splitlines() if line.strip())
    if not normalized:
        raise ValueError("allowlisted docs must not be empty or whitespace-only")
    if len(normalized) <= max_length:
        return normalized
    return normalized[: max_length - 3].rstrip() + "..."


def extract_title(content: str, *, source_path: str) -> tuple[str, bool]:
    match = FIRST_H1_PATTERN.search(content)
    if match:
        return match.group(1).strip(), True
    return _fallback_title(source_path), False


def deterministic_document_id(source_path: str, *, chunk_index: int = 0) -> str:
    return f"docs:{source_path}#{chunk_index}"


def deterministic_content_hash(source_path: str, content: str) -> str:
    digest = hashlib.sha256()
    digest.update(source_path.encode("utf-8"))
    digest.update(b"\0")
    digest.update(content.encode("utf-8"))
    return digest.hexdigest()


def get_repo_commit_hash(workspace_root: str | Path) -> str:
    root = Path(workspace_root).resolve()

    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            check=True,
            capture_output=True,
            text=True,
            cwd=root,
        )
    except (FileNotFoundError, subprocess.CalledProcessError):
        return _read_head_commit_from_git_dir(_resolve_git_dir(root))

    return result.stdout.strip()


def _load_document_payload(
    workspace_root: Path,
    source_path: str,
    *,
    repo_commit_hash: str,
) -> dict[str, Any]:
    file_path = workspace_root / PurePosixPath(source_path)
    content = _normalize_content(file_path.read_text(encoding="utf-8"))
    title, title_from_h1 = extract_title(content, source_path=source_path)
    preview_text = build_preview_text(content)
    source_updated_at = datetime.fromtimestamp(file_path.stat().st_mtime, tz=timezone.utc)

    return {
        "id": deterministic_document_id(source_path),
        "doc_kind": classify_doc_kind(source_path),
        "source_path": source_path,
        "title": title,
        "content": content,
        "preview_text": preview_text,
        "content_hash": deterministic_content_hash(source_path, content),
        "repo_commit_hash": repo_commit_hash,
        "source_updated_at": source_updated_at,
        "metadata": {
            "relative_path": source_path,
            "file_size_bytes": file_path.stat().st_size,
            "source_group": _source_group(source_path),
            "title_source": "h1" if title_from_h1 else "basename",
            "title_from_h1": title_from_h1,
        },
    }


def _fallback_title(source_path: str) -> str:
    stem = PurePosixPath(source_path).stem
    if stem.upper() == stem:
        return stem
    return stem.replace("-", " ").replace("_", " ").title()


def _source_group(source_path: str) -> str:
    if source_path.startswith(".agent/"):
        return "agent"
    first_segment = PurePosixPath(source_path).parts[0]
    if first_segment.endswith(".md"):
        return "root"
    return first_segment


def _normalize_content(content: str) -> str:
    return content.replace("\r\n", "\n").replace("\r", "\n")


def _resolve_git_dir(workspace_root: Path) -> Path:
    git_path = workspace_root / ".git"
    if git_path.is_dir():
        return git_path
    if git_path.is_file():
        content = git_path.read_text(encoding="utf-8").strip()
        prefix = "gitdir: "
        if content.startswith(prefix):
            resolved = Path(content[len(prefix) :])
            if not resolved.is_absolute():
                resolved = (workspace_root / resolved).resolve()
            return resolved
    raise FileNotFoundError(f"Unable to resolve git metadata under {workspace_root}")


def _read_head_commit_from_git_dir(git_dir: Path) -> str:
    head_content = (git_dir / "HEAD").read_text(encoding="utf-8").strip()
    ref_prefix = "ref: "
    if not head_content.startswith(ref_prefix):
        return head_content

    ref_name = head_content[len(ref_prefix) :]
    ref_path = git_dir / PurePosixPath(ref_name)
    if ref_path.exists():
        return ref_path.read_text(encoding="utf-8").strip()

    packed_refs_path = git_dir / "packed-refs"
    if packed_refs_path.exists():
        for line in packed_refs_path.read_text(encoding="utf-8").splitlines():
            stripped = line.strip()
            if not stripped or stripped.startswith("#") or stripped.startswith("^"):
                continue
            commit_hash, _, packed_ref_name = stripped.partition(" ")
            if packed_ref_name == ref_name:
                return commit_hash

    raise FileNotFoundError(f"Unable to resolve git HEAD ref {ref_name!r} from {git_dir}")


def _vector_literal(vector: Sequence[float]) -> str:
    return "[" + ",".join(format(value, ".17g") for value in vector) + "]"


def _embed_texts_in_batches(
    texts: Sequence[str],
    *,
    embedding_client: EmbeddingClient,
    max_batch_texts: int = EMBED_BATCH_MAX_TEXTS,
    max_batch_chars: int = EMBED_BATCH_MAX_CHARS,
) -> list[list[float]]:
    if not texts:
        return []

    embeddings: list[list[float]] = []
    batch: list[str] = []
    batch_chars = 0

    for text in texts:
        text_length = len(text)
        should_flush = bool(batch) and (
            len(batch) >= max_batch_texts or batch_chars + text_length > max_batch_chars
        )
        if should_flush:
            embeddings.extend(embedding_client.embed_texts(batch))
            batch = []
            batch_chars = 0

        batch.append(text)
        batch_chars += text_length

    if batch:
        embeddings.extend(embedding_client.embed_texts(batch))

    return embeddings


def _embed_document_content(
    content: str,
    *,
    embedding_client: EmbeddingClient,
    segment_target_chars: int = EMBED_SEGMENT_TARGET_CHARS,
) -> list[float]:
    segments = _split_text_for_embedding(content, max_chars=segment_target_chars)

    try:
        segment_embeddings = _embed_texts_in_batches(segments, embedding_client=embedding_client)
    except EmbeddingResponseError as error:
        if _is_context_length_error(error) and segment_target_chars > EMBED_SEGMENT_MIN_CHARS:
            next_target_chars = max(EMBED_SEGMENT_MIN_CHARS, segment_target_chars // 2)
            if next_target_chars == segment_target_chars:
                raise
            return _embed_document_content(
                content,
                embedding_client=embedding_client,
                segment_target_chars=next_target_chars,
            )
        raise

    return _aggregate_segment_embeddings(segments, segment_embeddings)


def _split_text_for_embedding(content: str, *, max_chars: int) -> list[str]:
    if max_chars <= 0:
        raise ValueError("max_chars must be positive")

    stripped_content = content.strip()
    if not stripped_content:
        raise ValueError("allowlisted docs must not be empty or whitespace-only")

    segments: list[str] = []
    start = 0
    content_length = len(content)

    while start < content_length:
        end = min(content_length, start + max_chars)
        split_at = end

        if end < content_length:
            newline_split = content.rfind("\n", start + 1, end + 1)
            if newline_split > start + (max_chars // 2):
                split_at = newline_split + 1

        if split_at <= start:
            split_at = end

        segment = content[start:split_at].strip()
        if segment:
            segments.append(segment)

        start = split_at

    if not segments:
        raise ValueError("allowlisted docs must not resolve to empty embedding segments")

    return segments


def _aggregate_segment_embeddings(
    segments: Sequence[str],
    segment_embeddings: Sequence[Sequence[float]],
) -> list[float]:
    if len(segments) != len(segment_embeddings):
        raise ValueError("segment and embedding counts must match")
    if not segments:
        raise ValueError("at least one segment is required")
    if len(segments) == 1:
        return list(segment_embeddings[0])

    dimension = len(segment_embeddings[0])
    weighted_sums = [0.0] * dimension
    total_weight = 0

    for segment, embedding in zip(segments, segment_embeddings, strict=True):
        if len(embedding) != dimension:
            raise ValueError("all segment embeddings must have the same dimension")
        weight = len(segment)
        total_weight += weight
        for index, value in enumerate(embedding):
            weighted_sums[index] += float(value) * weight

    if total_weight == 0:
        raise ValueError("segment weights must not be zero")

    return [value / total_weight for value in weighted_sums]


def _is_context_length_error(error: EmbeddingResponseError) -> bool:
    return "input length exceeds the context length" in str(error).lower()


def _load_psycopg() -> tuple[Any, Any]:
    import psycopg
    from psycopg.types.json import Json

    return psycopg, Json


class PostgresDocsStore:
    def __init__(self, connection: Any):
        self._connection = connection

    @classmethod
    def from_database_url(cls, database_url: str) -> "PostgresDocsStore":
        psycopg, _ = _load_psycopg()
        return cls(psycopg.connect(database_url))

    def __enter__(self) -> "PostgresDocsStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def upsert_documents(self, documents: Sequence[PreparedDocument]) -> int:
        if not documents:
            return 0

        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_documents (
                        id,
                        source_domain,
                        doc_kind,
                        source_path,
                        title,
                        section_title,
                        subsection_title,
                        heading_path,
                        chunk_index,
                        content,
                        preview_text,
                        content_hash,
                        repo_commit_hash,
                        source_updated_at,
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
                    ON CONFLICT (source_path, chunk_index) DO UPDATE SET
                        id = EXCLUDED.id,
                        source_domain = EXCLUDED.source_domain,
                        doc_kind = EXCLUDED.doc_kind,
                        title = EXCLUDED.title,
                        section_title = EXCLUDED.section_title,
                        subsection_title = EXCLUDED.subsection_title,
                        heading_path = EXCLUDED.heading_path,
                        content = EXCLUDED.content,
                        preview_text = EXCLUDED.preview_text,
                        content_hash = EXCLUDED.content_hash,
                        repo_commit_hash = EXCLUDED.repo_commit_hash,
                        source_updated_at = EXCLUDED.source_updated_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            document.id,
                            document.source_domain,
                            document.doc_kind,
                            document.source_path,
                            document.title,
                            document.section_title,
                            document.subsection_title,
                            Json(document.heading_path),
                            document.chunk_index,
                            document.content,
                            document.preview_text,
                            document.content_hash,
                            document.repo_commit_hash,
                            document.source_updated_at,
                            _vector_literal(document.embedding),
                            Json(document.metadata),
                        )
                        for document in documents
                    ],
                )

        return len(documents)

    def delete_documents_for_paths(self, source_paths: Sequence[str]) -> int:
        if not source_paths:
            return 0

        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.execute(
                    "DELETE FROM agentic.kb_documents WHERE source_path = ANY(%s)",
                    (list(source_paths),),
                )
                return cursor.rowcount

    def list_document_paths(self) -> list[str]:
        with self._connection.cursor() as cursor:
            cursor.execute(
                "SELECT DISTINCT source_path FROM agentic.kb_documents ORDER BY source_path"
            )
            return [row[0] for row in cursor.fetchall()]


class InMemoryDocsStore:
    def __init__(self):
        self.rows_by_key: dict[tuple[str, int], dict[str, Any]] = {}
        self.write_count = 0

    def upsert_documents(self, documents: Sequence[PreparedDocument]) -> int:
        for document in documents:
            self.write_count += 1
            key = (document.source_path, document.chunk_index)
            self.rows_by_key[key] = {
                "id": document.id,
                "source_domain": document.source_domain,
                "doc_kind": document.doc_kind,
                "source_path": document.source_path,
                "title": document.title,
                "section_title": document.section_title,
                "subsection_title": document.subsection_title,
                "heading_path": list(document.heading_path),
                "chunk_index": document.chunk_index,
                "content": document.content,
                "preview_text": document.preview_text,
                "content_hash": document.content_hash,
                "repo_commit_hash": document.repo_commit_hash,
                "source_updated_at": document.source_updated_at,
                "embedding": list(document.embedding),
                "metadata": dict(document.metadata),
                "updated_at_token": self.write_count,
            }
        return len(documents)

    def delete_documents_for_paths(self, source_paths: Sequence[str]) -> int:
        source_path_set = set(source_paths)
        to_delete = [key for key in self.rows_by_key if key[0] in source_path_set]
        for key in to_delete:
            del self.rows_by_key[key]
        return len(to_delete)

    def list_document_paths(self) -> list[str]:
        return sorted({key[0] for key in self.rows_by_key})
