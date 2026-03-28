from __future__ import annotations

import json
import re
from dataclasses import dataclass
from datetime import date, datetime, timezone
from typing import Any, Iterator, Protocol, Sequence
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

from agentic_kb.config import AgenticConfig
from agentic_kb.embed import OllamaEmbeddingClient
from agentic_kb.ingest.docs import (
    _embed_document_content,
    _load_psycopg,
    _normalize_content,
    _vector_literal,
    build_preview_text,
)
from agentic_kb.ingest.github import (
    deterministic_github_issue_id,
    deterministic_github_pr_id,
)
from agentic_kb.sync.state import (
    PostgresSyncStateStore,
    build_project_sync_state,
    build_sync_attempt,
    build_sync_failure,
    project_scope_key,
)


DEFAULT_GITHUB_PROJECT_OWNER = "DripDropz"
DEFAULT_GITHUB_PROJECT_NUMBER = 5
DEFAULT_GITHUB_REPO = "DripDropz/daedalus"
DEFAULT_PROJECT_PAGE_SIZE = 50
DEFAULT_FIELD_VALUES_PAGE_SIZE = 32
DEFAULT_REQUEST_TIMEOUT_SECONDS = 20
GITHUB_GRAPHQL_URL = "https://api.github.com/graphql"
GITHUB_API_VERSION = "2022-11-28"
PROJECT_PREVIEW_LENGTH = 280

CANONICAL_SINGLE_SELECT_FIELDS = {
    "Status": "status",
    "Priority": "priority",
    "Size": "size",
    "Work Type": "work_type",
    "Area": "area",
    "Phase": "phase",
    "KB Impact": "kb_impact",
}
CANONICAL_DATE_FIELDS = {
    "Start date": "start_date",
    "Target date": "target_date",
}
FIELD_SUMMARY_ORDER: tuple[str, ...] = (
    "Status",
    "Priority",
    "Size",
    "Work Type",
    "Area",
    "Phase",
    "KB Impact",
    "Start date",
    "Target date",
)
SNAKE_CASE_BOUNDARY = re.compile(r"(?<!^)(?=[A-Z])")

PROJECT_ITEMS_QUERY = """
query AgenticProjectItems(
  $projectOwner: String!
  $projectNumber: Int!
  $pageSize: Int!
  $afterCursor: String
  $fieldPageSize: Int!
) {
  organization(login: $projectOwner) {
    projectV2(number: $projectNumber) {
      id
      title
      url
      items(first: $pageSize, after: $afterCursor) {
        pageInfo {
          hasNextPage
          endCursor
        }
        nodes {
          id
          updatedAt
          isArchived
          fieldValues(first: $fieldPageSize) {
            pageInfo {
              hasNextPage
              endCursor
            }
            nodes {
              __typename
              ... on ProjectV2ItemFieldSingleSelectValue {
                name
                optionId
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
              ... on ProjectV2ItemFieldDateValue {
                date
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
              ... on ProjectV2ItemFieldTextValue {
                text
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
              ... on ProjectV2ItemFieldNumberValue {
                number
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
              ... on ProjectV2ItemFieldIterationValue {
                title
                startDate
                duration
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
              ... on ProjectV2ItemFieldLabelValue {
                labels(first: 20) {
                  nodes {
                    name
                  }
                }
                field {
                  ... on ProjectV2Field {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2IterationField {
                    id
                    name
                    dataType
                  }
                  ... on ProjectV2SingleSelectField {
                    id
                    name
                    dataType
                  }
                }
              }
            }
          }
          content {
            __typename
            ... on Issue {
              id
              title
              body
              url
              number
              state
              createdAt
              updatedAt
              closedAt
              repository {
                nameWithOwner
              }
              author {
                login
              }
            }
            ... on PullRequest {
              id
              title
              body
              url
              number
              state
              createdAt
              updatedAt
              closedAt
              mergedAt
              repository {
                nameWithOwner
              }
              author {
                login
              }
            }
            ... on DraftIssue {
              id
              title
              body
            }
          }
        }
      }
    }
  }
}
""".strip()


class ProjectIngestError(RuntimeError):
    pass


class MissingProjectGithubTokenError(ProjectIngestError):
    pass


class ProjectGithubApiError(ProjectIngestError):
    pass


class ProjectGithubApiResponseError(ProjectGithubApiError):
    pass


class ProjectGithubGraphQLError(ProjectGithubApiError):
    pass


class ProjectFieldPaginationError(ProjectGithubApiResponseError):
    pass


class EmbeddingClient(Protocol):
    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        ...


class ProjectItemsStore(Protocol):
    def upsert_project_items(self, items: Sequence["PreparedProjectItem"]) -> int:
        ...


@dataclass(frozen=True)
class PreparedProjectItem:
    id: str
    project_owner: str
    project_number: int
    project_item_node_id: str
    content_type: str | None
    content_id: str | None
    content_node_id: str | None
    title: str
    body_text: str
    repo: str | None
    status: str | None
    priority: str | None
    size: str | None
    work_type: str | None
    area: str | None
    phase: str | None
    kb_impact: str | None
    start_date: date | None
    target_date: date | None
    field_values: dict[str, Any]
    html_url: str | None
    source_updated_at: datetime
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class ProjectFetchBounds:
    project_owner: str = DEFAULT_GITHUB_PROJECT_OWNER
    project_number: int = DEFAULT_GITHUB_PROJECT_NUMBER
    page_size: int = DEFAULT_PROJECT_PAGE_SIZE
    max_pages: int | None = None
    after_cursor: str | None = None


@dataclass(frozen=True)
class ProjectItemsPage:
    project_owner: str
    project_number: int
    project_title: str | None
    project_url: str | None
    page_number: int
    end_cursor: str | None
    has_next_page: bool
    truncated_by_bound: bool
    latest_source_updated_at: datetime | None
    items: tuple[PreparedProjectItem, ...] = ()


@dataclass(frozen=True)
class ProjectPageWriteResult:
    project_owner: str
    project_number: int
    page_number: int
    end_cursor: str | None
    rows_written: int
    latest_source_updated_at: datetime | None


@dataclass(frozen=True)
class ProjectIngestResult:
    project_owner: str
    project_number: int
    project_title: str | None
    project_url: str | None
    bounds: ProjectFetchBounds
    pages_fetched: int
    hit_bound: bool
    final_cursor: str | None
    latest_source_updated_at: datetime | None
    rows_written: int


@dataclass(frozen=True)
class _NormalizedFieldValues:
    values_by_name: dict[str, Any]
    status: str | None = None
    priority: str | None = None
    size: str | None = None
    work_type: str | None = None
    area: str | None = None
    phase: str | None = None
    kb_impact: str | None = None
    start_date: date | None = None
    target_date: date | None = None

    def searchable_summary(self) -> str:
        lines: list[str] = []
        for field_name in FIELD_SUMMARY_ORDER:
            if field_name in CANONICAL_SINGLE_SELECT_FIELDS:
                value = getattr(self, CANONICAL_SINGLE_SELECT_FIELDS[field_name])
            else:
                value = getattr(self, CANONICAL_DATE_FIELDS[field_name])
                if isinstance(value, date):
                    value = value.isoformat()
            if value:
                lines.append(f"{field_name}: {value}")
        if not lines:
            return ""
        return "Project fields:\n" + "\n".join(lines)


class GithubProjectApiClient:
    def __init__(
        self,
        *,
        token: str,
        endpoint: str = GITHUB_GRAPHQL_URL,
        timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
        field_page_size: int = DEFAULT_FIELD_VALUES_PAGE_SIZE,
    ):
        if not token:
            raise MissingProjectGithubTokenError(
                "GITHUB_TOKEN is required for Project 5 ingestion"
            )

        self._token = token
        self._endpoint = endpoint
        self._timeout_seconds = timeout_seconds
        self._field_page_size = field_page_size

    def fetch_project_items_page(
        self,
        *,
        bounds: ProjectFetchBounds,
        after_cursor: str | None,
    ) -> dict[str, Any]:
        payload = self._request_json(
            PROJECT_ITEMS_QUERY,
            {
                "projectOwner": bounds.project_owner,
                "projectNumber": bounds.project_number,
                "pageSize": bounds.page_size,
                "afterCursor": after_cursor,
                "fieldPageSize": self._field_page_size,
            },
        )
        data = _require_graphql_data(payload)
        organization = _require_object(data.get("organization"), context="organization")
        project = _require_object(
            organization.get("projectV2"),
            context=(
                f"organization projectV2 for {bounds.project_owner}#{bounds.project_number}; "
                "verify the project exists and GITHUB_TOKEN can read organization projects"
            ),
        )
        items = _require_object(project.get("items"), context="project items connection")
        page_info = _require_object(items.get("pageInfo"), context="project items pageInfo")
        item_nodes = _require_list(items.get("nodes"), context="project items nodes")
        return {
            "project_id": _string_or_none(project.get("id")),
            "project_title": _string_or_none(project.get("title")),
            "project_url": _string_or_none(project.get("url")),
            "items": item_nodes,
            "has_next_page": bool(page_info.get("hasNextPage", False)),
            "end_cursor": _string_or_none(page_info.get("endCursor")),
        }

    def _request_json(self, query: str, variables: dict[str, Any]) -> Any:
        request = Request(
            self._endpoint,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self._token}",
                "Content-Type": "application/json",
                "X-GitHub-Api-Version": GITHUB_API_VERSION,
                "User-Agent": "agentic-kb-task-404",
            },
            data=json.dumps({"query": query, "variables": variables}).encode("utf-8"),
            method="POST",
        )

        try:
            with urlopen(request, timeout=self._timeout_seconds) as response:
                body = response.read().decode("utf-8")
        except HTTPError as error:
            detail = _read_http_error_detail(error)
            raise ProjectGithubApiError(
                f"GitHub GraphQL request failed with HTTP {error.code}: {detail}"
            ) from error
        except (TimeoutError, URLError, OSError) as error:
            raise ProjectGithubApiError(
                f"Unable to reach GitHub GraphQL endpoint {self._endpoint}: {error}"
            ) from error

        try:
            return json.loads(body)
        except json.JSONDecodeError as error:
            raise ProjectGithubApiResponseError(
                f"GitHub GraphQL returned invalid JSON: {error}"
            ) from error


def deterministic_project_item_id(
    project_owner: str,
    project_number: int,
    project_item_node_id: str,
) -> str:
    return f"github-project-item:{project_owner}/{project_number}:{project_item_node_id}"


def build_project_preview_text(
    content: str,
    *,
    max_length: int = PROJECT_PREVIEW_LENGTH,
) -> str:
    return build_preview_text(content, max_length=max_length)


def iter_project_item_pages(
    *,
    bounds: ProjectFetchBounds | None = None,
    embedding_client: EmbeddingClient,
    github_client: GithubProjectApiClient,
) -> Iterator[ProjectItemsPage]:
    active_bounds = bounds or ProjectFetchBounds()
    after_cursor = active_bounds.after_cursor
    page_number = 1

    while True:
        payload = github_client.fetch_project_items_page(
            bounds=active_bounds,
            after_cursor=after_cursor,
        )
        project_title = _string_or_none(payload.get("project_title"))
        project_url = _string_or_none(payload.get("project_url"))
        has_next_page = bool(payload.get("has_next_page", False))
        end_cursor = _string_or_none(payload.get("end_cursor"))
        if has_next_page and not end_cursor:
            raise ProjectGithubApiResponseError(
                "Project items page indicated hasNextPage without an endCursor"
            )

        items = tuple(
            _prepare_project_item(
                _require_object(item_payload, context="project item node"),
                project_owner=active_bounds.project_owner,
                project_number=active_bounds.project_number,
                project_title=project_title,
                project_url=project_url,
                embedding_client=embedding_client,
            )
            for item_payload in _require_list(payload.get("items"), context="project item nodes")
        )
        latest_source_updated_at = _max_datetime(
            *(item.source_updated_at for item in items)
        )
        truncated_by_bound = bool(
            active_bounds.max_pages is not None
            and has_next_page
            and page_number >= active_bounds.max_pages
        )

        yield ProjectItemsPage(
            project_owner=active_bounds.project_owner,
            project_number=active_bounds.project_number,
            project_title=project_title,
            project_url=project_url,
            page_number=page_number,
            end_cursor=end_cursor,
            has_next_page=has_next_page,
            truncated_by_bound=truncated_by_bound,
            latest_source_updated_at=latest_source_updated_at,
            items=items,
        )

        if truncated_by_bound or not has_next_page:
            break
        after_cursor = end_cursor
        page_number += 1


def write_project_item_page(
    page: ProjectItemsPage,
    *,
    project_store: ProjectItemsStore,
) -> ProjectPageWriteResult:
    rows_written = project_store.upsert_project_items(page.items)
    return ProjectPageWriteResult(
        project_owner=page.project_owner,
        project_number=page.project_number,
        page_number=page.page_number,
        end_cursor=page.end_cursor,
        rows_written=rows_written,
        latest_source_updated_at=page.latest_source_updated_at,
    )


def ingest_project_items(
    *,
    embedding_client: EmbeddingClient,
    project_store: ProjectItemsStore,
    bounds: ProjectFetchBounds | None = None,
    github_token: str | None = None,
    github_client: GithubProjectApiClient | None = None,
    request_timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
) -> ProjectIngestResult:
    active_bounds = bounds or ProjectFetchBounds()
    client = github_client or GithubProjectApiClient(
        token=github_token or "",
        timeout_seconds=request_timeout_seconds,
    )
    pages_fetched = 0
    hit_bound = False
    final_cursor = active_bounds.after_cursor
    latest_source_updated_at: datetime | None = None
    rows_written = 0
    project_title: str | None = None
    project_url: str | None = None

    for page in iter_project_item_pages(
        bounds=active_bounds,
        embedding_client=embedding_client,
        github_client=client,
    ):
        write_result = write_project_item_page(page, project_store=project_store)
        pages_fetched += 1
        hit_bound = hit_bound or page.truncated_by_bound
        final_cursor = write_result.end_cursor
        latest_source_updated_at = _max_datetime(
            latest_source_updated_at,
            write_result.latest_source_updated_at,
        )
        rows_written += write_result.rows_written
        project_title = page.project_title
        project_url = page.project_url

    return ProjectIngestResult(
        project_owner=active_bounds.project_owner,
        project_number=active_bounds.project_number,
        project_title=project_title,
        project_url=project_url,
        bounds=active_bounds,
        pages_fetched=pages_fetched,
        hit_bound=hit_bound,
        final_cursor=final_cursor,
        latest_source_updated_at=latest_source_updated_at,
        rows_written=rows_written,
    )


def ingest_project_items_from_config(
    *,
    config: AgenticConfig | None = None,
    bounds: ProjectFetchBounds | None = None,
    request_timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
) -> ProjectIngestResult:
    active_config = config or AgenticConfig.from_env()
    if not active_config.database_url:
        raise ValueError("DATABASE_URL is required for project ingestion")
    if not active_config.github_token:
        raise MissingProjectGithubTokenError(
            "GITHUB_TOKEN is required for Project 5 ingestion"
        )

    embedding_client = OllamaEmbeddingClient.from_config(active_config)
    active_bounds = bounds or ProjectFetchBounds()
    attempted_at = datetime.now(timezone.utc)
    scope_key = project_scope_key(
        active_bounds.project_owner,
        active_bounds.project_number,
    )
    with PostgresProjectItemsStore.from_database_url(active_config.database_url) as project_store:
        with PostgresSyncStateStore.from_database_url(active_config.database_url) as sync_store:
            sync_store.record_attempts(
                [
                    build_sync_attempt(
                        "project",
                        scope_key,
                        attempted_at=attempted_at,
                        metadata={
                            "project_owner": active_bounds.project_owner,
                            "project_number": active_bounds.project_number,
                        },
                    )
                ]
            )
            try:
                result = ingest_project_items(
                    embedding_client=embedding_client,
                    project_store=project_store,
                    bounds=active_bounds,
                    github_token=active_config.github_token,
                    request_timeout_seconds=request_timeout_seconds,
                )
            except Exception as error:
                sync_store.record_failures(
                    [
                        build_sync_failure(
                            "project",
                            scope_key,
                            attempted_at=attempted_at,
                            error=error,
                            metadata={
                                "project_owner": active_bounds.project_owner,
                                "project_number": active_bounds.project_number,
                            },
                        )
                    ]
                )
                raise

            sync_store.upsert_sync_states(
                [
                    build_project_sync_state(
                        result,
                        attempted_at=attempted_at,
                        succeeded_at=datetime.now(timezone.utc),
                    )
                ]
            )
            return result


def _prepare_project_item(
    payload: dict[str, Any],
    *,
    project_owner: str,
    project_number: int,
    project_title: str | None,
    project_url: str | None,
    embedding_client: EmbeddingClient,
) -> PreparedProjectItem:
    project_item_node_id = _require_text(payload.get("id"), context="project item id")
    source_updated_at = _parse_timestamp(
        payload.get("updatedAt"),
        context=f"project item {project_item_node_id} updatedAt",
    )
    field_values_connection = _require_object(
        payload.get("fieldValues"), context="project item fieldValues"
    )
    field_page_info = _require_object(
        field_values_connection.get("pageInfo"), context="project item fieldValues.pageInfo"
    )
    if bool(field_page_info.get("hasNextPage", False)):
        raise ProjectFieldPaginationError(
            "Project item fieldValues exceeded the requested page size; nested field-value pagination is not supported yet"
        )
    raw_field_values = _require_list(
        field_values_connection.get("nodes"), context="project item fieldValues.nodes"
    )
    normalized_fields = _normalize_field_values(raw_field_values)

    content_payload = payload.get("content")
    content_type, content_node_id, repo, html_url, title, raw_body, metadata = _normalize_content_payload(
        content_payload,
        project_item_node_id=project_item_node_id,
    )
    field_summary = normalized_fields.searchable_summary()
    body_text = _build_project_body_text(title=title, raw_body=raw_body, field_summary=field_summary)
    content_id = _determine_content_id(content_type=content_type, repo=repo, metadata=metadata)
    metadata.update(
        {
            "project_title": project_title,
            "project_url": project_url,
            "is_archived": bool(payload.get("isArchived", False)),
            "raw_content_typename": _string_or_none(
                _require_object(content_payload, context="content").get("__typename")
            )
            if isinstance(content_payload, dict)
            else None,
            "field_page_end_cursor": _string_or_none(field_page_info.get("endCursor")),
        }
    )

    return PreparedProjectItem(
        id=deterministic_project_item_id(project_owner, project_number, project_item_node_id),
        project_owner=project_owner,
        project_number=project_number,
        project_item_node_id=project_item_node_id,
        content_type=content_type,
        content_id=content_id,
        content_node_id=content_node_id,
        title=title,
        body_text=body_text,
        repo=repo,
        status=normalized_fields.status,
        priority=normalized_fields.priority,
        size=normalized_fields.size,
        work_type=normalized_fields.work_type,
        area=normalized_fields.area,
        phase=normalized_fields.phase,
        kb_impact=normalized_fields.kb_impact,
        start_date=normalized_fields.start_date,
        target_date=normalized_fields.target_date,
        field_values=normalized_fields.values_by_name,
        html_url=html_url,
        source_updated_at=source_updated_at,
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata=metadata,
    )


def _normalize_field_values(raw_nodes: Sequence[Any]) -> _NormalizedFieldValues:
    values_by_name: dict[str, Any] = {}
    typed_values: dict[str, Any] = {
        "status": None,
        "priority": None,
        "size": None,
        "work_type": None,
        "area": None,
        "phase": None,
        "kb_impact": None,
        "start_date": None,
        "target_date": None,
    }

    for index, raw_node in enumerate(raw_nodes, start=1):
        node = _require_object(raw_node, context="project field value node")
        value_type = _require_text(node.get("__typename"), context="project field value typename")
        field_details = _extract_field_details(node.get("field"))
        field_name = field_details["name"] or f"unnamed_field_{index}"
        raw_value = _extract_field_value_payload(node, value_type=value_type)
        values_by_name[field_name] = {
            "type": value_type,
            "field_node_id": field_details["id"],
            "field_data_type": field_details["data_type"],
            **raw_value,
        }

        if field_name in CANONICAL_SINGLE_SELECT_FIELDS:
            if value_type != "ProjectV2ItemFieldSingleSelectValue":
                raise ProjectGithubApiResponseError(
                    f"Canonical project field {field_name!r} expected a single-select value, got {value_type!r}"
                )
            selected_name = _string_or_none(raw_value.get("value"))
            if not selected_name:
                raise ProjectGithubApiResponseError(
                    f"Canonical project field {field_name!r} was missing a selected option name"
                )
            typed_values[CANONICAL_SINGLE_SELECT_FIELDS[field_name]] = selected_name
        elif field_name in CANONICAL_DATE_FIELDS:
            if value_type != "ProjectV2ItemFieldDateValue":
                raise ProjectGithubApiResponseError(
                    f"Canonical project field {field_name!r} expected a date value, got {value_type!r}"
                )
            raw_date = _string_or_none(raw_value.get("value"))
            if not raw_date:
                raise ProjectGithubApiResponseError(
                    f"Canonical project field {field_name!r} was missing a date value"
                )
            typed_values[CANONICAL_DATE_FIELDS[field_name]] = _parse_date(
                raw_date,
                context=f"project field {field_name}",
            )

    return _NormalizedFieldValues(values_by_name=values_by_name, **typed_values)


def _extract_field_details(value: Any) -> dict[str, str | None]:
    if value is None:
        return {"id": None, "name": None, "data_type": None}
    field = _require_object(value, context="project field")
    return {
        "id": _string_or_none(field.get("id")),
        "name": _string_or_none(field.get("name")),
        "data_type": _string_or_none(field.get("dataType")),
    }


def _extract_field_value_payload(node: dict[str, Any], *, value_type: str) -> dict[str, Any]:
    if value_type == "ProjectV2ItemFieldSingleSelectValue":
        return {
            "value": _string_or_none(node.get("name")),
            "option_id": _string_or_none(node.get("optionId")),
        }
    if value_type == "ProjectV2ItemFieldDateValue":
        return {"value": _string_or_none(node.get("date"))}
    if value_type == "ProjectV2ItemFieldTextValue":
        return {"value": _string_or_none(node.get("text"))}
    if value_type == "ProjectV2ItemFieldNumberValue":
        return {"value": _number_or_none(node.get("number"))}
    if value_type == "ProjectV2ItemFieldIterationValue":
        return {
            "value": _string_or_none(node.get("title")),
            "start_date": _string_or_none(node.get("startDate")),
            "duration": _int_or_none(node.get("duration"), context="iteration duration"),
        }
    if value_type == "ProjectV2ItemFieldLabelValue":
        labels = _require_object(node.get("labels"), context="field labels")
        label_nodes = _require_list(labels.get("nodes"), context="field labels.nodes")
        return {
            "value": [
                _require_text(
                    _require_object(label, context="field label").get("name"),
                    context="field label name",
                )
                for label in label_nodes
            ]
        }
    return {}


def _normalize_content_payload(
    value: Any,
    *,
    project_item_node_id: str,
) -> tuple[
    str | None,
    str | None,
    str | None,
    str | None,
    str,
    str,
    dict[str, Any],
]:
    if value is None:
        title = _fallback_project_item_title(project_item_node_id)
        return None, None, None, None, title, "", {"content_missing": True}

    content = _require_object(value, context="project item content")
    typename = _require_text(content.get("__typename"), context="project item content typename")
    content_type = _snake_case_typename(typename)
    content_node_id = _string_or_none(content.get("id"))
    title = _string_or_none(content.get("title")) or _fallback_project_item_title(project_item_node_id)
    raw_body = _normalize_text(content.get("body"))
    html_url = _string_or_none(content.get("url"))
    repo = None
    repo_payload = content.get("repository")
    if repo_payload is not None:
        repo = _string_or_none(
            _require_object(repo_payload, context="project item content repository").get(
                "nameWithOwner"
            )
        )

    metadata = {
        "content_number": _int_or_none(content.get("number"), context="project item content number"),
        "content_state": _string_or_none(content.get("state")),
        "content_author_login": _user_login(content.get("author")),
        "content_created_at": _string_or_none(content.get("createdAt")),
        "content_updated_at": _string_or_none(content.get("updatedAt")),
        "content_closed_at": _string_or_none(content.get("closedAt")),
        "content_merged_at": _string_or_none(content.get("mergedAt")),
    }
    return content_type, content_node_id, repo, html_url, title, raw_body, metadata


def _determine_content_id(
    *,
    content_type: str | None,
    repo: str | None,
    metadata: dict[str, Any],
) -> str | None:
    if repo != DEFAULT_GITHUB_REPO:
        return None
    content_number = metadata.get("content_number")
    if not isinstance(content_number, int):
        return None
    if content_type == "issue":
        return deterministic_github_issue_id(repo, content_number)
    if content_type == "pull_request":
        return deterministic_github_pr_id(repo, content_number)
    return None


def _build_project_body_text(*, title: str, raw_body: str, field_summary: str) -> str:
    parts = [title]
    if raw_body:
        parts.append(raw_body)
    if field_summary:
        parts.append(field_summary)
    return "\n\n".join(parts)


def _fallback_project_item_title(project_item_node_id: str) -> str:
    return f"Project item {project_item_node_id}"


def _snake_case_typename(value: str) -> str:
    return SNAKE_CASE_BOUNDARY.sub("_", value).lower()


def _require_graphql_data(payload: Any) -> dict[str, Any]:
    response = _require_object(payload, context="GraphQL response")
    errors = response.get("errors")
    if errors:
        if not isinstance(errors, list):
            raise ProjectGithubApiResponseError("GraphQL errors payload must be a list")
        messages = []
        for raw_error in errors:
            error = _require_object(raw_error, context="GraphQL error")
            messages.append(_require_text(error.get("message"), context="GraphQL error message"))
        raise ProjectGithubGraphQLError("; ".join(messages))
    return _require_object(response.get("data"), context="GraphQL data")


def _require_object(value: Any, *, context: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise ProjectGithubApiResponseError(f"Expected object for {context}")
    return value


def _require_list(value: Any, *, context: str) -> list[Any]:
    if not isinstance(value, list):
        raise ProjectGithubApiResponseError(f"Expected list for {context}")
    return value


def _require_text(value: Any, *, context: str) -> str:
    if not isinstance(value, str):
        raise ProjectGithubApiResponseError(f"Expected string for {context}")
    normalized = _normalize_text(value)
    if not normalized:
        raise ProjectGithubApiResponseError(f"Expected non-empty string for {context}")
    return normalized


def _normalize_text(value: Any) -> str:
    if value is None:
        return ""
    if not isinstance(value, str):
        raise ProjectGithubApiResponseError(f"Expected text field, got {type(value).__name__}")
    return _normalize_content(value).strip()


def _string_or_none(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, str):
        raise ProjectGithubApiResponseError(f"Expected string field, got {type(value).__name__}")
    normalized = _normalize_content(value).strip()
    return normalized or None


def _int_or_none(value: Any, *, context: str) -> int | None:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, int):
        raise ProjectGithubApiResponseError(f"Expected integer for {context}")
    return value


def _number_or_none(value: Any) -> float | int | None:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ProjectGithubApiResponseError("Expected number field for project value")
    return value


def _user_login(value: Any) -> str | None:
    if value is None:
        return None
    user = _require_object(value, context="user")
    return _string_or_none(user.get("login"))


def _parse_timestamp(value: Any, *, context: str) -> datetime:
    if not isinstance(value, str) or not value:
        raise ProjectGithubApiResponseError(f"Missing or invalid timestamp for {context}")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise ProjectGithubApiResponseError(
            f"Invalid timestamp for {context}: {value!r}"
        ) from error
    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def _parse_date(value: str, *, context: str) -> date:
    try:
        return date.fromisoformat(value)
    except ValueError as error:
        raise ProjectGithubApiResponseError(
            f"Invalid date for {context}: {value!r}"
        ) from error


def _max_datetime(*values: datetime | None) -> datetime | None:
    resolved = [value for value in values if value is not None]
    if not resolved:
        return None
    return max(resolved)


def _read_http_error_detail(error: HTTPError) -> str:
    try:
        raw_detail = error.read().decode("utf-8")
    except Exception:
        raw_detail = ""
    if not raw_detail:
        return str(error.reason or "request failed")
    try:
        payload = json.loads(raw_detail)
    except json.JSONDecodeError:
        return raw_detail
    if isinstance(payload, dict) and isinstance(payload.get("message"), str):
        return payload["message"]
    return raw_detail


class PostgresProjectItemsStore:
    def __init__(self, connection: Any):
        self._connection = connection

    @classmethod
    def from_database_url(cls, database_url: str) -> "PostgresProjectItemsStore":
        psycopg, _ = _load_psycopg()
        return cls(psycopg.connect(database_url))

    def __enter__(self) -> "PostgresProjectItemsStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def upsert_project_items(self, items: Sequence[PreparedProjectItem]) -> int:
        if not items:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_project_items (
                        id,
                        project_owner,
                        project_number,
                        project_item_node_id,
                        content_type,
                        content_id,
                        content_node_id,
                        title,
                        body_text,
                        repo,
                        status,
                        priority,
                        size,
                        work_type,
                        area,
                        phase,
                        kb_impact,
                        start_date,
                        target_date,
                        field_values,
                        html_url,
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
                    ON CONFLICT (project_item_node_id) DO UPDATE SET
                        id = EXCLUDED.id,
                        project_owner = EXCLUDED.project_owner,
                        project_number = EXCLUDED.project_number,
                        content_type = EXCLUDED.content_type,
                        content_id = EXCLUDED.content_id,
                        content_node_id = EXCLUDED.content_node_id,
                        title = EXCLUDED.title,
                        body_text = EXCLUDED.body_text,
                        repo = EXCLUDED.repo,
                        status = EXCLUDED.status,
                        priority = EXCLUDED.priority,
                        size = EXCLUDED.size,
                        work_type = EXCLUDED.work_type,
                        area = EXCLUDED.area,
                        phase = EXCLUDED.phase,
                        kb_impact = EXCLUDED.kb_impact,
                        start_date = EXCLUDED.start_date,
                        target_date = EXCLUDED.target_date,
                        field_values = EXCLUDED.field_values,
                        html_url = EXCLUDED.html_url,
                        source_updated_at = EXCLUDED.source_updated_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            item.id,
                            item.project_owner,
                            item.project_number,
                            item.project_item_node_id,
                            item.content_type,
                            item.content_id,
                            item.content_node_id,
                            item.title,
                            item.body_text,
                            item.repo,
                            item.status,
                            item.priority,
                            item.size,
                            item.work_type,
                            item.area,
                            item.phase,
                            item.kb_impact,
                            item.start_date,
                            item.target_date,
                            Json(item.field_values),
                            item.html_url,
                            item.source_updated_at,
                            _vector_literal(item.embedding),
                            Json(item.metadata),
                        )
                        for item in items
                    ],
                )
        return len(items)


class InMemoryProjectItemsStore:
    def __init__(self):
        self.rows_by_key: dict[str, dict[str, Any]] = {}
        self.write_count = 0

    def upsert_project_items(self, items: Sequence[PreparedProjectItem]) -> int:
        for item in items:
            self.write_count += 1
            self.rows_by_key[item.project_item_node_id] = {
                **item.__dict__,
                "field_values": dict(item.field_values),
                "embedding": list(item.embedding),
                "metadata": dict(item.metadata),
                "updated_at_token": self.write_count,
            }
        return len(items)
