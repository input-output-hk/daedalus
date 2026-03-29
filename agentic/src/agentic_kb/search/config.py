from __future__ import annotations

from dataclasses import dataclass
from enum import StrEnum
from types import MappingProxyType
from typing import Mapping


class SearchSourceDomain(StrEnum):
    DOCS = "docs"
    CODE = "code"
    GITHUB = "github"
    PROJECT = "project"


class SearchEntityType(StrEnum):
    DOCUMENTS = "documents"
    CODE_CHUNKS = "code_chunks"
    GITHUB_ISSUES = "github_issues"
    GITHUB_ISSUE_COMMENTS = "github_issue_comments"
    GITHUB_PRS = "github_prs"
    GITHUB_PR_COMMENTS = "github_pr_comments"
    PROJECT_ITEMS = "project_items"


class SearchFilterScope(StrEnum):
    GLOBAL = "global"
    ENTITY = "entity"


class SearchFilterValueType(StrEnum):
    TEXT = "text"
    INTEGER = "integer"
    DATE = "date"


class SearchFilterMatchType(StrEnum):
    EXACT = "exact"
    PREFIX = "prefix"


@dataclass(frozen=True)
class SearchFilterConfig:
    key: str
    scope: SearchFilterScope
    value_type: SearchFilterValueType
    match_type: SearchFilterMatchType = SearchFilterMatchType.EXACT
    column_name: str | None = None
    metadata_key: str | None = None
    description: str = ""

    def __post_init__(self) -> None:
        if not self.key:
            raise ValueError("Search filter keys must be non-empty")
        if self.scope == SearchFilterScope.ENTITY and not self.column_name and not self.metadata_key:
            raise ValueError(
                f"Entity search filter {self.key!r} must define a backing column name or metadata key"
            )
        if self.scope == SearchFilterScope.GLOBAL and self.column_name is not None:
            raise ValueError(
                f"Global search filter {self.key!r} cannot define a backing column name"
            )
        if self.scope == SearchFilterScope.GLOBAL and self.metadata_key is not None:
            raise ValueError(
                f"Global search filter {self.key!r} cannot define a backing metadata key"
            )
        if self.column_name is not None and self.metadata_key is not None:
            raise ValueError(
                f"Entity search filter {self.key!r} cannot define both a backing column name and metadata key"
            )
        if (
            self.match_type == SearchFilterMatchType.PREFIX
            and self.value_type != SearchFilterValueType.TEXT
        ):
            raise ValueError(
                f"Prefix search filter {self.key!r} must use text values"
            )


@dataclass(frozen=True)
class SearchEntityConfig:
    entity_type: SearchEntityType
    source_domain: SearchSourceDomain
    table_name: str
    primary_key_column: str
    text_columns: tuple[str, ...]
    preview_column: str
    result_columns: tuple[str, ...]
    embedding_column: str
    bm25_columns: tuple[str, ...]
    filters: tuple[SearchFilterConfig, ...]

    def __post_init__(self) -> None:
        if not self.table_name:
            raise ValueError("Search entity configs must define a table name")
        if self.primary_key_column not in self.result_columns:
            raise ValueError(
                f"Search entity {self.entity_type} must expose its primary key column in result columns"
            )
        if self.preview_column not in self.result_columns:
            raise ValueError(
                f"Search entity {self.entity_type} must expose its preview column in result columns"
            )
        if self.embedding_column not in {"embedding"}:
            raise ValueError(
                f"Search entity {self.entity_type} must use the standard embedding column"
            )
        if not self.text_columns:
            raise ValueError(
                f"Search entity {self.entity_type} must define at least one text column"
            )
        duplicate_filter_keys = _duplicate_keys(filter_config.key for filter_config in self.filters)
        if duplicate_filter_keys:
            raise ValueError(
                f"Search entity {self.entity_type} defines duplicate filters: {', '.join(duplicate_filter_keys)}"
            )
        invalid_scopes = [
            filter_config.key
            for filter_config in self.filters
            if filter_config.scope != SearchFilterScope.ENTITY
        ]
        if invalid_scopes:
            raise ValueError(
                f"Search entity {self.entity_type} contains non-entity filters: {', '.join(invalid_scopes)}"
            )


def _duplicate_keys(keys: list[str] | tuple[str, ...] | object) -> tuple[str, ...]:
    counts: dict[str, int] = {}
    for key in keys:
        counts[str(key)] = counts.get(str(key), 0) + 1
    return tuple(sorted(key for key, count in counts.items() if count > 1))


def _global_filter(key: str, *, description: str) -> SearchFilterConfig:
    return SearchFilterConfig(
        key=key,
        scope=SearchFilterScope.GLOBAL,
        value_type=SearchFilterValueType.TEXT,
        description=description,
    )


def _entity_filter(
    key: str,
    *,
    column_name: str | None = None,
    metadata_key: str | None = None,
    description: str,
    value_type: SearchFilterValueType = SearchFilterValueType.TEXT,
    match_type: SearchFilterMatchType = SearchFilterMatchType.EXACT,
) -> SearchFilterConfig:
    return SearchFilterConfig(
        key=key,
        scope=SearchFilterScope.ENTITY,
        value_type=value_type,
        match_type=match_type,
        column_name=column_name,
        metadata_key=metadata_key,
        description=description,
    )


GLOBAL_SEARCH_FILTERS: tuple[SearchFilterConfig, ...] = (
    _global_filter(
        "entity_type",
        description="Restrict search to one or more configured entity types before per-entity SQL is built.",
    ),
)


_ENTITY_CONFIGS: tuple[SearchEntityConfig, ...] = (
    SearchEntityConfig(
        entity_type=SearchEntityType.DOCUMENTS,
        source_domain=SearchSourceDomain.DOCS,
        table_name="agentic.kb_documents",
        primary_key_column="id",
        text_columns=("title", "preview_text", "content"),
        preview_column="preview_text",
        result_columns=("id", "source_path", "title", "preview_text", "doc_kind"),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "source_domain",
            "doc_kind",
            "source_path",
            "title",
            "preview_text",
            "content",
        ),
        filters=(
            _entity_filter(
                "doc_kind",
                column_name="doc_kind",
                description="Restrict document results to a specific document kind.",
            ),
            _entity_filter(
                "source_path_prefix",
                column_name="source_path",
                match_type=SearchFilterMatchType.PREFIX,
                description="Restrict document results to a repo-relative source path prefix.",
            ),
            _entity_filter(
                "task_id",
                metadata_key="task_id",
                description="Restrict document results to a canonical task plan id.",
            ),
            _entity_filter(
                "planning_status",
                metadata_key="planning_status",
                description="Restrict document results to a canonical task plan planning status.",
            ),
            _entity_filter(
                "build_status",
                metadata_key="build_status",
                description="Restrict document results to a canonical task plan build status.",
            ),
            _entity_filter(
                "plan_type",
                metadata_key="plan_type",
                description="Restrict document results to a plan artifact classification.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.CODE_CHUNKS,
        source_domain=SearchSourceDomain.CODE,
        table_name="agentic.kb_code_chunks",
        primary_key_column="id",
        text_columns=("symbol_name", "preview_text", "content"),
        preview_column="preview_text",
        result_columns=(
            "id",
            "repo_path",
            "symbol_name",
            "symbol_kind",
            "preview_text",
            "start_line",
            "end_line",
        ),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "repo_path",
            "language",
            "symbol_name",
            "symbol_kind",
            "preview_text",
            "content",
        ),
        filters=(
            _entity_filter(
                "repo_path_prefix",
                column_name="repo_path",
                match_type=SearchFilterMatchType.PREFIX,
                description="Restrict code results to a repo-relative path prefix.",
            ),
            _entity_filter(
                "language",
                column_name="language",
                description="Restrict code results to a language family such as typescript or typescriptreact.",
            ),
            _entity_filter(
                "symbol_kind",
                column_name="symbol_kind",
                description="Restrict code results to a symbol kind such as class, function, or field_function.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.GITHUB_ISSUES,
        source_domain=SearchSourceDomain.GITHUB,
        table_name="agentic.kb_github_issues",
        primary_key_column="id",
        text_columns=("title", "preview_text", "body_text"),
        preview_column="preview_text",
        result_columns=(
            "id",
            "repo",
            "issue_number",
            "title",
            "preview_text",
            "state",
            "html_url",
        ),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "repo",
            "issue_number",
            "state",
            "title",
            "preview_text",
            "body_text",
        ),
        filters=(
            _entity_filter(
                "repo",
                column_name="repo",
                description="Restrict GitHub issue results to a repository identifier.",
            ),
            _entity_filter(
                "state",
                column_name="state",
                description="Restrict GitHub issue results by open or closed state.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.GITHUB_ISSUE_COMMENTS,
        source_domain=SearchSourceDomain.GITHUB,
        table_name="agentic.kb_github_issue_comments",
        primary_key_column="id",
        text_columns=("preview_text", "body_text"),
        preview_column="preview_text",
        result_columns=(
            "id",
            "repo",
            "issue_number",
            "preview_text",
            "html_url",
        ),
        embedding_column="embedding",
        bm25_columns=("id", "repo", "issue_number", "preview_text", "body_text"),
        filters=(
            _entity_filter(
                "repo",
                column_name="repo",
                description="Restrict GitHub issue-comment results to a repository identifier.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.GITHUB_PRS,
        source_domain=SearchSourceDomain.GITHUB,
        table_name="agentic.kb_github_prs",
        primary_key_column="id",
        text_columns=("title", "preview_text", "body_text"),
        preview_column="preview_text",
        result_columns=(
            "id",
            "repo",
            "pr_number",
            "title",
            "preview_text",
            "state",
            "html_url",
        ),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "repo",
            "pr_number",
            "state",
            "title",
            "preview_text",
            "body_text",
        ),
        filters=(
            _entity_filter(
                "repo",
                column_name="repo",
                description="Restrict GitHub pull-request results to a repository identifier.",
            ),
            _entity_filter(
                "state",
                column_name="state",
                description="Restrict GitHub pull-request results by open or closed state.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.GITHUB_PR_COMMENTS,
        source_domain=SearchSourceDomain.GITHUB,
        table_name="agentic.kb_github_pr_comments",
        primary_key_column="id",
        text_columns=("preview_text", "body_text"),
        preview_column="preview_text",
        result_columns=(
            "id",
            "repo",
            "pr_number",
            "comment_type",
            "repo_path",
            "preview_text",
            "html_url",
        ),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "repo",
            "pr_number",
            "comment_type",
            "repo_path",
            "preview_text",
            "body_text",
        ),
        filters=(
            _entity_filter(
                "repo",
                column_name="repo",
                description="Restrict GitHub pull-request comment results to a repository identifier.",
            ),
            _entity_filter(
                "comment_type",
                column_name="comment_type",
                description="Restrict GitHub PR comment results to issue_comment or review_comment.",
            ),
        ),
    ),
    SearchEntityConfig(
        entity_type=SearchEntityType.PROJECT_ITEMS,
        source_domain=SearchSourceDomain.PROJECT,
        table_name="agentic.kb_project_items",
        primary_key_column="id",
        text_columns=("title", "body_text"),
        preview_column="body_text",
        result_columns=(
            "id",
            "project_owner",
            "project_number",
            "repo",
            "title",
            "body_text",
            "content_type",
            "status",
            "priority",
            "size",
            "work_type",
            "area",
            "phase",
            "kb_impact",
        ),
        embedding_column="embedding",
        bm25_columns=(
            "id",
            "project_owner",
            "project_number",
            "content_type",
            "repo",
            "status",
            "priority",
            "work_type",
            "area",
            "phase",
            "kb_impact",
            "title",
            "body_text",
        ),
        filters=(
            _entity_filter(
                "repo",
                column_name="repo",
                description="Restrict project items to linked repository content.",
            ),
            _entity_filter(
                "status",
                column_name="status",
                description="Restrict project items by Project 5 status.",
            ),
            _entity_filter(
                "priority",
                column_name="priority",
                description="Restrict project items by Project 5 priority.",
            ),
            _entity_filter(
                "size",
                column_name="size",
                description="Restrict project items by Project 5 size.",
            ),
            _entity_filter(
                "work_type",
                column_name="work_type",
                description="Restrict project items by Project 5 work type.",
            ),
            _entity_filter(
                "area",
                column_name="area",
                description="Restrict project items by Project 5 area.",
            ),
            _entity_filter(
                "phase",
                column_name="phase",
                description="Restrict project items by Project 5 phase.",
            ),
            _entity_filter(
                "kb_impact",
                column_name="kb_impact",
                description="Restrict project items by KB impact.",
            ),
            _entity_filter(
                "content_type",
                column_name="content_type",
                description="Restrict project items by linked content type.",
            ),
        ),
    ),
)


_duplicate_entity_types = _duplicate_keys([config.entity_type for config in _ENTITY_CONFIGS])
if _duplicate_entity_types:
    raise ValueError(
        f"Duplicate search entity types are not allowed: {', '.join(_duplicate_entity_types)}"
    )


SEARCH_ENTITY_REGISTRY: Mapping[SearchEntityType, SearchEntityConfig] = MappingProxyType(
    {config.entity_type: config for config in _ENTITY_CONFIGS}
)


def get_search_entity_config(
    entity_type: str | SearchEntityType,
) -> SearchEntityConfig:
    normalized = SearchEntityType(entity_type)
    return SEARCH_ENTITY_REGISTRY[normalized]


def list_search_entity_configs() -> tuple[SearchEntityConfig, ...]:
    return tuple(SEARCH_ENTITY_REGISTRY.values())


def list_global_filters() -> tuple[SearchFilterConfig, ...]:
    return GLOBAL_SEARCH_FILTERS


def list_supported_filters(
    entity_type: str | SearchEntityType | None = None,
) -> tuple[SearchFilterConfig, ...]:
    if entity_type is None:
        return GLOBAL_SEARCH_FILTERS
    config = get_search_entity_config(entity_type)
    return GLOBAL_SEARCH_FILTERS + config.filters


def list_searchable_tables() -> tuple[str, ...]:
    return tuple(config.table_name for config in SEARCH_ENTITY_REGISTRY.values())


def entity_types_for_source_domain(
    source_domain: str | SearchSourceDomain,
) -> tuple[SearchEntityType, ...]:
    normalized = SearchSourceDomain(source_domain)
    return tuple(
        config.entity_type
        for config in SEARCH_ENTITY_REGISTRY.values()
        if config.source_domain == normalized
    )
