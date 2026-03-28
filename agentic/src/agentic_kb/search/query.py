from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date
from enum import StrEnum
from math import inf
import re
from typing import Any, Mapping, Protocol, Sequence

from agentic_kb.embed import EXPECTED_EMBEDDING_DIMENSION, OllamaEmbeddingClient
from agentic_kb.search.config import (
    SEARCH_ENTITY_REGISTRY,
    SearchEntityConfig,
    SearchEntityType,
    SearchFilterConfig,
    SearchFilterMatchType,
    SearchFilterScope,
    SearchFilterValueType,
)


DEFAULT_SEARCH_LIMIT = 10
DEFAULT_RRF_K = 60
DEFAULT_CANDIDATE_MULTIPLIER = 5
MIN_CANDIDATE_POOL = 20
_IDENTIFIER_PATTERN = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


class SearchMode(StrEnum):
    BM25 = "bm25"
    VECTOR = "vector"
    HYBRID = "hybrid"


class SearchQueryError(RuntimeError):
    pass


class SearchValidationError(SearchQueryError):
    pass


class QueryEmbeddingProvider(Protocol):
    def embed_text(self, text: str) -> list[float]:
        ...


@dataclass(frozen=True)
class SearchRequest:
    query_text: str
    limit: int = DEFAULT_SEARCH_LIMIT
    mode: SearchMode = SearchMode.HYBRID
    filters: Mapping[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not isinstance(self.query_text, str):
            raise SearchValidationError("query_text must be a string")
        normalized_query = self.query_text.strip()
        if not normalized_query:
            raise SearchValidationError("query_text must not be empty or whitespace-only")
        if isinstance(self.limit, bool) or not isinstance(self.limit, int) or self.limit <= 0:
            raise SearchValidationError("limit must be a positive integer")

        try:
            normalized_mode = SearchMode(self.mode)
        except ValueError as error:
            raise SearchValidationError(
                f"mode must be one of: {', '.join(mode.value for mode in SearchMode)}"
            ) from error

        if not isinstance(self.filters, Mapping):
            raise SearchValidationError("filters must be a mapping")

        normalized_filters = dict(self.filters)
        object.__setattr__(self, "query_text", normalized_query)
        object.__setattr__(self, "mode", normalized_mode)
        object.__setattr__(self, "filters", normalized_filters)


@dataclass(frozen=True)
class SearchHit:
    entity_type: SearchEntityType
    source_domain: str
    id: str
    fields: Mapping[str, Any]
    bm25_score: float | None = None
    vector_distance: float | None = None
    fused_score: float | None = None
    bm25_rank: int | None = None
    vector_rank: int | None = None
    hybrid_rank: int | None = None


@dataclass(frozen=True)
class SearchResultSet:
    query_text: str
    mode: SearchMode
    limit: int
    filters: Mapping[str, Any]
    entity_types: tuple[SearchEntityType, ...]
    hits: tuple[SearchHit, ...]


@dataclass(frozen=True)
class CompiledSql:
    sql: str
    params: tuple[Any, ...]


@dataclass(frozen=True)
class _RowHit:
    entity_type: SearchEntityType
    source_domain: str
    id: str
    fields: Mapping[str, Any]
    bm25_score: float | None = None
    vector_distance: float | None = None


class PostgresSearchStore:
    def __init__(
        self,
        connection: Any,
        *,
        embedding_provider: QueryEmbeddingProvider | None = None,
        rrf_k: int = DEFAULT_RRF_K,
        candidate_multiplier: int = DEFAULT_CANDIDATE_MULTIPLIER,
        min_candidate_pool: int = MIN_CANDIDATE_POOL,
    ):
        self._connection = connection
        self._embedding_provider = embedding_provider
        self._rrf_k = rrf_k
        self._candidate_multiplier = candidate_multiplier
        self._min_candidate_pool = min_candidate_pool

    @classmethod
    def from_database_url(
        cls,
        database_url: str,
        *,
        embedding_provider: QueryEmbeddingProvider | None = None,
    ) -> "PostgresSearchStore":
        psycopg = _load_psycopg()
        return cls(psycopg.connect(database_url), embedding_provider=embedding_provider)

    @classmethod
    def from_config(cls, config: Any) -> "PostgresSearchStore":
        if not config.database_url:
            raise SearchValidationError("DATABASE_URL is required for search queries")
        return cls.from_database_url(
            config.database_url,
            embedding_provider=OllamaEmbeddingClient.from_config(config),
        )

    def __enter__(self) -> "PostgresSearchStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def search(
        self,
        request: SearchRequest,
        *,
        query_embedding: Sequence[float] | None = None,
    ) -> SearchResultSet:
        selected_configs = select_search_entity_configs(request.filters)

        if request.mode == SearchMode.BM25:
            hits = self._search_bm25(request, selected_configs)
        elif request.mode == SearchMode.VECTOR:
            embedding = _coerce_query_embedding(
                query_embedding,
                query_text=request.query_text,
                embedding_provider=self._embedding_provider,
            )
            hits = self._search_vector(request, selected_configs, query_embedding=embedding)
        else:
            embedding = _coerce_query_embedding(
                query_embedding,
                query_text=request.query_text,
                embedding_provider=self._embedding_provider,
            )
            hits = self._search_hybrid(request, selected_configs, query_embedding=embedding)

        return SearchResultSet(
            query_text=request.query_text,
            mode=request.mode,
            limit=request.limit,
            filters=dict(request.filters),
            entity_types=tuple(config.entity_type for config in selected_configs),
            hits=tuple(hits),
        )

    def search_bm25(self, request: SearchRequest) -> SearchResultSet:
        return self.search(_with_mode(request, SearchMode.BM25))

    def search_vector(
        self,
        request: SearchRequest,
        *,
        query_embedding: Sequence[float] | None = None,
    ) -> SearchResultSet:
        return self.search(_with_mode(request, SearchMode.VECTOR), query_embedding=query_embedding)

    def search_hybrid(
        self,
        request: SearchRequest,
        *,
        query_embedding: Sequence[float] | None = None,
    ) -> SearchResultSet:
        return self.search(_with_mode(request, SearchMode.HYBRID), query_embedding=query_embedding)

    def _search_bm25(
        self,
        request: SearchRequest,
        selected_configs: Sequence[SearchEntityConfig],
    ) -> list[SearchHit]:
        row_hits: list[_RowHit] = []
        for config in selected_configs:
            compiled = build_bm25_entity_query(
                config,
                request.query_text,
                request.filters,
                candidate_limit=self._candidate_limit(request.limit),
                final_limit=request.limit,
            )
            row_hits.extend(self._execute_row_hits(compiled, config))
        return _rank_bm25_hits(row_hits, limit=request.limit)

    def _search_vector(
        self,
        request: SearchRequest,
        selected_configs: Sequence[SearchEntityConfig],
        *,
        query_embedding: Sequence[float],
    ) -> list[SearchHit]:
        row_hits: list[_RowHit] = []
        for config in selected_configs:
            compiled = build_vector_entity_query(
                config,
                query_embedding,
                request.filters,
                limit=request.limit,
            )
            row_hits.extend(self._execute_row_hits(compiled, config))
        return _rank_vector_hits(row_hits, limit=request.limit)

    def _search_hybrid(
        self,
        request: SearchRequest,
        selected_configs: Sequence[SearchEntityConfig],
        *,
        query_embedding: Sequence[float],
    ) -> list[SearchHit]:
        candidate_limit = self._candidate_limit(request.limit)
        bm25_hits: list[_RowHit] = []
        vector_hits: list[_RowHit] = []

        for config in selected_configs:
            bm25_compiled = build_bm25_entity_query(
                config,
                request.query_text,
                request.filters,
                candidate_limit=candidate_limit,
                final_limit=candidate_limit,
            )
            vector_compiled = build_vector_entity_query(
                config,
                query_embedding,
                request.filters,
                limit=candidate_limit,
            )
            bm25_hits.extend(self._execute_row_hits(bm25_compiled, config))
            vector_hits.extend(self._execute_row_hits(vector_compiled, config))

        return merge_hybrid_hits(
            bm25_hits,
            vector_hits,
            limit=request.limit,
            rrf_k=self._rrf_k,
        )

    def _candidate_limit(self, final_limit: int) -> int:
        return max(final_limit * self._candidate_multiplier, final_limit, self._min_candidate_pool)

    def _execute_row_hits(
        self,
        compiled: CompiledSql,
        config: SearchEntityConfig,
    ) -> list[_RowHit]:
        with self._connection.cursor() as cursor:
            cursor.execute(compiled.sql, compiled.params)
            rows = cursor.fetchall()

        hits: list[_RowHit] = []
        for row in rows:
            fields = {
                column_name: row[index]
                for index, column_name in enumerate(config.result_columns)
            }
            hits.append(
                _RowHit(
                    entity_type=config.entity_type,
                    source_domain=config.source_domain.value,
                    id=str(fields[config.primary_key_column]),
                    fields=fields,
                    bm25_score=row[len(config.result_columns)],
                    vector_distance=row[len(config.result_columns) + 1],
                )
            )
        return hits


def search_bm25(
    search_store: PostgresSearchStore,
    request: SearchRequest,
) -> SearchResultSet:
    return search_store.search_bm25(request)


def search_vector(
    search_store: PostgresSearchStore,
    request: SearchRequest,
    *,
    query_embedding: Sequence[float] | None = None,
) -> SearchResultSet:
    return search_store.search_vector(request, query_embedding=query_embedding)


def search_hybrid(
    search_store: PostgresSearchStore,
    request: SearchRequest,
    *,
    query_embedding: Sequence[float] | None = None,
) -> SearchResultSet:
    return search_store.search_hybrid(request, query_embedding=query_embedding)


def select_search_entity_configs(filters: Mapping[str, Any]) -> tuple[SearchEntityConfig, ...]:
    explicit_entity_types = _normalize_entity_type_filter(filters.get("entity_type"))
    selected = [
        config
        for config in SEARCH_ENTITY_REGISTRY.values()
        if explicit_entity_types is None or config.entity_type in explicit_entity_types
    ]

    requested_entity_filter_keys = tuple(
        key for key in filters if key not in _global_filter_keys()
    )
    supported_filter_keys = _all_supported_entity_filter_keys()
    unsupported_filter_keys = sorted(
        key for key in requested_entity_filter_keys if key not in supported_filter_keys
    )
    if unsupported_filter_keys:
        raise SearchValidationError(
            f"Unsupported search filters: {', '.join(unsupported_filter_keys)}"
        )

    if requested_entity_filter_keys:
        selected = [
            config
            for config in selected
            if all(_entity_filter_for_key(config, key) is not None for key in requested_entity_filter_keys)
        ]

    if not selected:
        if explicit_entity_types is not None:
            raise SearchValidationError(
                "Selected entity_type values do not support the requested filters"
            )
        raise SearchValidationError("No searchable entity types match the requested filters")

    return tuple(selected)


def build_bm25_entity_query(
    config: SearchEntityConfig,
    query_text: str,
    filters: Mapping[str, Any],
    *,
    candidate_limit: int,
    final_limit: int,
) -> CompiledSql:
    predicate_sql, predicate_params = _build_bm25_predicate(config, query_text)
    filter_sql, filter_params = build_entity_filter_clause(config, filters, table_alias="base")
    candidate_columns = ", ".join(
        f"{_quote_identifier(column_name)}"
        for column_name in config.result_columns
    )
    primary_key = _quote_identifier(config.primary_key_column)
    table_name = _quote_qualified_name(config.table_name)
    score_sql = f"paradedb.score({primary_key})"
    sql = (
        "WITH bm25_candidates AS ("
        f" SELECT {primary_key} AS candidate_id, {score_sql} AS bm25_score"
        f" FROM {table_name}"
        f" WHERE {predicate_sql}"
        f" ORDER BY {score_sql} DESC, {primary_key} ASC"
        " LIMIT %s"
        ")"
        f" SELECT {candidate_columns}, bm25_candidates.bm25_score, NULL::double precision AS vector_distance"
        " FROM bm25_candidates"
        f" JOIN {table_name} AS base ON base.{primary_key} = bm25_candidates.candidate_id"
        f"{' WHERE ' + filter_sql if filter_sql else ''}"
        f" ORDER BY bm25_candidates.bm25_score DESC, base.{primary_key} ASC"
        " LIMIT %s"
    )
    return CompiledSql(
        sql=sql,
        params=predicate_params + (candidate_limit,) + filter_params + (final_limit,),
    )


def build_vector_entity_query(
    config: SearchEntityConfig,
    query_embedding: Sequence[float],
    filters: Mapping[str, Any],
    *,
    limit: int,
) -> CompiledSql:
    filter_sql, filter_params = build_entity_filter_clause(config, filters, table_alias="base")
    candidate_columns = ", ".join(
        f"base.{_quote_identifier(column_name)}"
        for column_name in config.result_columns
    )
    primary_key = _quote_identifier(config.primary_key_column)
    embedding_column = _quote_identifier(config.embedding_column)
    table_name = _quote_qualified_name(config.table_name)
    sql = (
        f"SELECT {candidate_columns}, NULL::double precision AS bm25_score, "
        f"base.{embedding_column} <=> %s::vector AS vector_distance"
        f" FROM {table_name} AS base"
        f" WHERE base.{embedding_column} IS NOT NULL{' AND ' + filter_sql if filter_sql else ''}"
        f" ORDER BY base.{embedding_column} <=> %s::vector ASC, base.{primary_key} ASC"
        " LIMIT %s"
    )
    vector_literal = _vector_literal(query_embedding)
    return CompiledSql(
        sql=sql,
        params=(vector_literal,) + filter_params + (vector_literal, limit),
    )


def build_entity_filter_clause(
    config: SearchEntityConfig,
    filters: Mapping[str, Any],
    *,
    table_alias: str,
) -> tuple[str, tuple[Any, ...]]:
    clauses: list[str] = []
    params: list[Any] = []

    for key, raw_value in filters.items():
        if key in _global_filter_keys():
            continue
        filter_config = _entity_filter_for_key(config, key)
        if filter_config is None:
            continue

        column_sql = f"{table_alias}.{_quote_identifier(filter_config.column_name or '')}"
        coerced_value = _coerce_filter_value(filter_config, raw_value)
        if filter_config.match_type == SearchFilterMatchType.PREFIX:
            clauses.append(f"{column_sql} LIKE %s")
            params.append(f"{coerced_value}%")
        else:
            clauses.append(f"{column_sql} = %s")
            params.append(coerced_value)

    if not clauses:
        return "", ()
    return " AND ".join(clauses), tuple(params)


def merge_hybrid_hits(
    bm25_hits: Sequence[_RowHit],
    vector_hits: Sequence[_RowHit],
    *,
    limit: int,
    rrf_k: int = DEFAULT_RRF_K,
) -> list[SearchHit]:
    ranked_bm25 = _rank_bm25_hits(bm25_hits, limit=None)
    ranked_vector = _rank_vector_hits(vector_hits, limit=None)

    merged: dict[tuple[SearchEntityType, str], SearchHit] = {}
    for hit in ranked_bm25:
        key = (hit.entity_type, hit.id)
        merged[key] = SearchHit(
            entity_type=hit.entity_type,
            source_domain=hit.source_domain,
            id=hit.id,
            fields=hit.fields,
            bm25_score=hit.bm25_score,
            vector_distance=hit.vector_distance,
            bm25_rank=hit.bm25_rank,
            vector_rank=hit.vector_rank,
            fused_score=_rrf_score(hit.bm25_rank, rrf_k),
        )

    for hit in ranked_vector:
        key = (hit.entity_type, hit.id)
        existing = merged.get(key)
        if existing is None:
            merged[key] = SearchHit(
                entity_type=hit.entity_type,
                source_domain=hit.source_domain,
                id=hit.id,
                fields=hit.fields,
                bm25_score=hit.bm25_score,
                vector_distance=hit.vector_distance,
                bm25_rank=hit.bm25_rank,
                vector_rank=hit.vector_rank,
                fused_score=_rrf_score(hit.vector_rank, rrf_k),
            )
            continue

        merged[key] = SearchHit(
            entity_type=existing.entity_type,
            source_domain=existing.source_domain,
            id=existing.id,
            fields=existing.fields,
            bm25_score=existing.bm25_score,
            vector_distance=hit.vector_distance,
            bm25_rank=existing.bm25_rank,
            vector_rank=hit.vector_rank,
            fused_score=(existing.fused_score or 0.0) + _rrf_score(hit.vector_rank, rrf_k),
        )

    ordered = sorted(merged.values(), key=_hybrid_sort_key)
    ranked_hits: list[SearchHit] = []
    for index, hit in enumerate(ordered[:limit], start=1):
        ranked_hits.append(
            SearchHit(
                entity_type=hit.entity_type,
                source_domain=hit.source_domain,
                id=hit.id,
                fields=hit.fields,
                bm25_score=hit.bm25_score,
                vector_distance=hit.vector_distance,
                fused_score=hit.fused_score,
                bm25_rank=hit.bm25_rank,
                vector_rank=hit.vector_rank,
                hybrid_rank=index,
            )
        )
    return ranked_hits


def _rank_bm25_hits(row_hits: Sequence[_RowHit], *, limit: int | None) -> list[SearchHit]:
    ordered = sorted(
        row_hits,
        key=lambda hit: (
            -(hit.bm25_score if hit.bm25_score is not None else float("-inf")),
            hit.entity_type.value,
            hit.id,
        ),
    )
    ranked_hits: list[SearchHit] = []
    for index, row_hit in enumerate(ordered, start=1):
        ranked_hits.append(
            SearchHit(
                entity_type=row_hit.entity_type,
                source_domain=row_hit.source_domain,
                id=row_hit.id,
                fields=row_hit.fields,
                bm25_score=row_hit.bm25_score,
                vector_distance=row_hit.vector_distance,
                bm25_rank=index,
            )
        )
    if limit is None:
        return ranked_hits
    return ranked_hits[:limit]


def _rank_vector_hits(row_hits: Sequence[_RowHit], *, limit: int | None) -> list[SearchHit]:
    ordered = sorted(
        row_hits,
        key=lambda hit: (
            hit.vector_distance if hit.vector_distance is not None else inf,
            hit.entity_type.value,
            hit.id,
        ),
    )
    ranked_hits: list[SearchHit] = []
    for index, row_hit in enumerate(ordered, start=1):
        ranked_hits.append(
            SearchHit(
                entity_type=row_hit.entity_type,
                source_domain=row_hit.source_domain,
                id=row_hit.id,
                fields=row_hit.fields,
                bm25_score=row_hit.bm25_score,
                vector_distance=row_hit.vector_distance,
                vector_rank=index,
            )
        )
    if limit is None:
        return ranked_hits
    return ranked_hits[:limit]


def _hybrid_sort_key(hit: SearchHit) -> tuple[Any, ...]:
    best_rank = min(rank for rank in (hit.bm25_rank, hit.vector_rank) if rank is not None)
    return (
        -(hit.fused_score or 0.0),
        best_rank,
        hit.bm25_rank if hit.bm25_rank is not None else inf,
        hit.vector_rank if hit.vector_rank is not None else inf,
        hit.entity_type.value,
        hit.id,
    )


def _rrf_score(rank: int | None, rrf_k: int) -> float:
    if rank is None:
        return 0.0
    return 1.0 / (rrf_k + rank)


def _with_mode(request: SearchRequest, mode: SearchMode) -> SearchRequest:
    return SearchRequest(
        query_text=request.query_text,
        limit=request.limit,
        mode=mode,
        filters=request.filters,
    )


def _coerce_query_embedding(
    query_embedding: Sequence[float] | None,
    *,
    query_text: str,
    embedding_provider: QueryEmbeddingProvider | None,
) -> list[float]:
    if query_embedding is not None:
        embedding = [float(value) for value in query_embedding]
    elif embedding_provider is None:
        raise SearchValidationError(
            "An embedding provider or query_embedding is required for vector and hybrid search"
        )
    else:
        embedding = [float(value) for value in embedding_provider.embed_text(query_text)]

    if len(embedding) != EXPECTED_EMBEDDING_DIMENSION:
        raise SearchValidationError(
            f"query embedding must have dimension {EXPECTED_EMBEDDING_DIMENSION}"
        )
    return embedding


def _build_bm25_predicate(
    config: SearchEntityConfig,
    query_text: str,
) -> tuple[str, tuple[Any, ...]]:
    predicates = [
        f"{_quote_identifier(column_name)} @@@ %s"
        for column_name in config.text_columns
    ]
    return "(" + " OR ".join(predicates) + ")", tuple(query_text for _ in config.text_columns)


def _coerce_filter_value(filter_config: SearchFilterConfig, raw_value: Any) -> Any:
    if filter_config.value_type == SearchFilterValueType.TEXT:
        if not isinstance(raw_value, str):
            raise SearchValidationError(f"Filter {filter_config.key!r} must be a string")
        normalized = raw_value.strip()
        if not normalized:
            raise SearchValidationError(
                f"Filter {filter_config.key!r} must not be empty or whitespace-only"
            )
        return normalized
    if filter_config.value_type == SearchFilterValueType.INTEGER:
        if isinstance(raw_value, bool) or not isinstance(raw_value, int):
            raise SearchValidationError(f"Filter {filter_config.key!r} must be an integer")
        return raw_value
    if filter_config.value_type == SearchFilterValueType.DATE:
        if isinstance(raw_value, date):
            return raw_value
        if isinstance(raw_value, str):
            try:
                return date.fromisoformat(raw_value)
            except ValueError as error:
                raise SearchValidationError(
                    f"Filter {filter_config.key!r} must use YYYY-MM-DD date format"
                ) from error
        raise SearchValidationError(f"Filter {filter_config.key!r} must be a date or ISO date string")
    raise SearchValidationError(f"Unsupported filter value type for {filter_config.key!r}")


def _normalize_entity_type_filter(value: Any) -> tuple[SearchEntityType, ...] | None:
    if value is None:
        return None

    raw_values: Sequence[Any]
    if isinstance(value, (str, SearchEntityType)):
        raw_values = [value]
    elif isinstance(value, Sequence):
        raw_values = value
    else:
        raise SearchValidationError("entity_type filter must be a string or sequence of strings")

    normalized: list[SearchEntityType] = []
    for raw_value in raw_values:
        try:
            normalized.append(SearchEntityType(raw_value))
        except ValueError as error:
            raise SearchValidationError(
                f"Unsupported entity_type value: {raw_value!r}"
            ) from error

    if not normalized:
        raise SearchValidationError("entity_type filter must not be empty")
    return tuple(dict.fromkeys(normalized))


def _entity_filter_for_key(
    config: SearchEntityConfig,
    key: str,
) -> SearchFilterConfig | None:
    for filter_config in config.filters:
        if filter_config.scope == SearchFilterScope.ENTITY and filter_config.key == key:
            return filter_config
    return None


def _global_filter_keys() -> set[str]:
    return {"entity_type"}


def _all_supported_entity_filter_keys() -> set[str]:
    keys: set[str] = set()
    for config in SEARCH_ENTITY_REGISTRY.values():
        keys.update(filter_config.key for filter_config in config.filters)
    return keys


def _quote_identifier(identifier: str) -> str:
    if not _IDENTIFIER_PATTERN.match(identifier):
        raise SearchValidationError(f"Unsafe SQL identifier: {identifier!r}")
    return f'"{identifier}"'


def _quote_qualified_name(name: str) -> str:
    parts = name.split(".")
    if not parts:
        raise SearchValidationError(f"Unsafe SQL name: {name!r}")
    return ".".join(_quote_identifier(part) for part in parts)


def _vector_literal(values: Sequence[float]) -> str:
    return "[" + ",".join(format(float(value), ".17g") for value in values) + "]"


def _load_psycopg() -> Any:
    import psycopg

    return psycopg
