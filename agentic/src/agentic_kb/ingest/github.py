from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from datetime import datetime, timezone
from typing import Any, Iterator, Protocol, Sequence
from urllib.error import HTTPError, URLError
from urllib.parse import parse_qs, urlencode, urljoin, urlparse
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
from agentic_kb.sync.state import (
    PostgresSyncStateStore,
    build_github_sync_state_updates,
    build_sync_attempt,
    build_sync_failure,
    github_scope_key,
)


DEFAULT_GITHUB_REPO = "DripDropz/daedalus"
DEFAULT_PAGE_SIZE = 100
DEFAULT_REQUEST_TIMEOUT_SECONDS = 20
GITHUB_API_BASE_URL = "https://api.github.com/"
GITHUB_API_VERSION = "2022-11-28"
GITHUB_PREVIEW_LENGTH = 280
STREAM_ORDER: tuple[str, ...] = (
    "issues",
    "pulls",
    "issue_comments",
    "review_comments",
)
ISSUE_NUMBER_PATTERN = re.compile(r"/issues/(\d+)$")
PULL_NUMBER_PATTERN = re.compile(r"/pulls/(\d+)$")


class GithubIngestError(RuntimeError):
    pass


class MissingGithubTokenError(GithubIngestError):
    pass


class GithubApiError(GithubIngestError):
    pass


class GithubApiResponseError(GithubApiError):
    pass


class GithubPaginationError(GithubApiError):
    pass


class GithubParentRoutingError(GithubIngestError):
    pass


class EmbeddingClient(Protocol):
    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        ...


class GithubIssuesStore(Protocol):
    def upsert_issues(self, issues: Sequence["PreparedGithubIssue"]) -> int:
        ...


class GithubIssueCommentsStore(Protocol):
    def upsert_issue_comments(
        self, comments: Sequence["PreparedGithubIssueComment"]
    ) -> int:
        ...


class GithubPullRequestsStore(Protocol):
    def upsert_prs(self, pull_requests: Sequence["PreparedGithubPullRequest"]) -> int:
        ...


class GithubPrCommentsStore(Protocol):
    def upsert_pr_comments(self, comments: Sequence["PreparedGithubPrComment"]) -> int:
        ...


class GithubStore(
    GithubIssuesStore,
    GithubIssueCommentsStore,
    GithubPullRequestsStore,
    GithubPrCommentsStore,
    Protocol,
):
    pass


@dataclass(frozen=True)
class PreparedGithubIssue:
    id: str
    repo: str
    issue_number: int
    github_node_id: str | None
    title: str
    state: str
    author_login: str | None
    labels: list[str]
    body_text: str
    preview_text: str
    html_url: str
    source_created_at: datetime
    source_updated_at: datetime
    source_closed_at: datetime | None
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class PreparedGithubIssueComment:
    id: str
    issue_id: str
    repo: str
    issue_number: int
    github_comment_id: int
    github_node_id: str | None
    author_login: str | None
    body_text: str
    preview_text: str
    html_url: str
    source_created_at: datetime
    source_updated_at: datetime
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class PreparedGithubPullRequest:
    id: str
    repo: str
    pr_number: int
    github_node_id: str | None
    title: str
    state: str
    author_login: str | None
    base_branch: str | None
    head_branch: str | None
    labels: list[str]
    body_text: str
    preview_text: str
    html_url: str
    source_created_at: datetime
    source_updated_at: datetime
    source_closed_at: datetime | None
    source_merged_at: datetime | None
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class PreparedGithubPrComment:
    id: str
    pr_id: str
    repo: str
    pr_number: int
    comment_type: str
    github_comment_id: int
    github_node_id: str | None
    author_login: str | None
    repo_path: str | None
    commit_oid: str | None
    original_commit_oid: str | None
    diff_hunk: str | None
    line: int | None
    original_line: int | None
    side: str | None
    start_line: int | None
    start_side: str | None
    body_text: str
    preview_text: str
    html_url: str
    source_created_at: datetime
    source_updated_at: datetime
    embedding: list[float]
    metadata: dict[str, Any]


@dataclass(frozen=True)
class GithubFetchBounds:
    repo: str = DEFAULT_GITHUB_REPO
    updated_since: datetime | None = None
    page_size: int = DEFAULT_PAGE_SIZE
    max_pages: int | None = None
    max_pages_per_stream: dict[str, int] | None = None

    def page_limit_for_stream(self, stream_name: str) -> int | None:
        if self.max_pages_per_stream and stream_name in self.max_pages_per_stream:
            return self.max_pages_per_stream[stream_name]
        return self.max_pages


@dataclass(frozen=True)
class GithubPageBatch:
    stream_name: str
    page_number: int
    has_next_page: bool
    truncated_by_bound: bool
    latest_source_updated_at: datetime | None
    issues: tuple[PreparedGithubIssue, ...] = ()
    issue_comments: tuple[PreparedGithubIssueComment, ...] = ()
    pull_requests: tuple[PreparedGithubPullRequest, ...] = ()
    pr_comments: tuple[PreparedGithubPrComment, ...] = ()


@dataclass(frozen=True)
class GithubPageWriteResult:
    stream_name: str
    page_number: int
    issues_written: int
    issue_comments_written: int
    prs_written: int
    pr_comments_written: int
    latest_source_updated_at: datetime | None


@dataclass(frozen=True)
class GithubStreamProgress:
    stream_name: str
    pages_fetched: int
    hit_bound: bool
    latest_source_updated_at: datetime | None
    issues_written: int
    issue_comments_written: int
    prs_written: int
    pr_comments_written: int


@dataclass(frozen=True)
class GithubIngestResult:
    repo: str
    bounds: GithubFetchBounds
    stream_progress: dict[str, GithubStreamProgress]
    issues_written: int
    issue_comments_written: int
    prs_written: int
    pr_comments_written: int


@dataclass
class _GithubParentCache:
    repo: str
    issues_by_number: dict[int, PreparedGithubIssue] = field(default_factory=dict)
    prs_by_number: dict[int, PreparedGithubPullRequest] = field(default_factory=dict)


@dataclass
class _MutableStreamProgress:
    stream_name: str
    pages_fetched: int = 0
    hit_bound: bool = False
    latest_source_updated_at: datetime | None = None
    issues_written: int = 0
    issue_comments_written: int = 0
    prs_written: int = 0
    pr_comments_written: int = 0

    def observe_batch(self, batch: GithubPageBatch, write_result: GithubPageWriteResult) -> None:
        self.pages_fetched += 1
        self.hit_bound = self.hit_bound or batch.truncated_by_bound
        self.latest_source_updated_at = _max_datetime(
            self.latest_source_updated_at,
            write_result.latest_source_updated_at,
        )
        self.issues_written += write_result.issues_written
        self.issue_comments_written += write_result.issue_comments_written
        self.prs_written += write_result.prs_written
        self.pr_comments_written += write_result.pr_comments_written

    def freeze(self) -> GithubStreamProgress:
        return GithubStreamProgress(
            stream_name=self.stream_name,
            pages_fetched=self.pages_fetched,
            hit_bound=self.hit_bound,
            latest_source_updated_at=self.latest_source_updated_at,
            issues_written=self.issues_written,
            issue_comments_written=self.issue_comments_written,
            prs_written=self.prs_written,
            pr_comments_written=self.pr_comments_written,
        )


class GithubApiClient:
    def __init__(
        self,
        *,
        token: str,
        base_url: str = GITHUB_API_BASE_URL,
        timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
    ):
        if not token:
            raise MissingGithubTokenError("GITHUB_TOKEN is required for GitHub ingestion")

        self._token = token
        self._base_url = base_url.rstrip("/") + "/"
        self._timeout_seconds = timeout_seconds

    def fetch_stream_page(
        self,
        stream_name: str,
        *,
        bounds: GithubFetchBounds,
        page_number: int,
    ) -> tuple[list[dict[str, Any]], bool]:
        url = self._stream_page_url(stream_name, bounds=bounds, page_number=page_number)
        payload, headers = self._request_json(url)
        if not isinstance(payload, list):
            raise GithubApiResponseError(
                f"GitHub stream {stream_name!r} page {page_number} returned a non-list payload"
            )
        return [self._require_object(item, context=f"{stream_name} item") for item in payload], bool(
            _parse_next_link(headers.get("Link"))
        )

    def fetch_issue_payload(self, issue_url: str) -> dict[str, Any]:
        payload, _ = self._request_json(issue_url)
        return self._require_object(payload, context=f"issue payload from {issue_url}")

    def fetch_pull_request(self, repo: str, pr_number: int) -> dict[str, Any]:
        payload, _ = self._request_json(f"repos/{repo}/pulls/{pr_number}")
        return self._require_object(payload, context=f"pull request {repo}#{pr_number}")

    def _stream_page_url(
        self,
        stream_name: str,
        *,
        bounds: GithubFetchBounds,
        page_number: int,
    ) -> str:
        endpoint = {
            "issues": f"repos/{bounds.repo}/issues",
            "pulls": f"repos/{bounds.repo}/pulls",
            "issue_comments": f"repos/{bounds.repo}/issues/comments",
            "review_comments": f"repos/{bounds.repo}/pulls/comments",
        }.get(stream_name)
        if endpoint is None:
            raise GithubPaginationError(f"Unknown GitHub stream {stream_name!r}")

        query = {
            "state": "all",
            "sort": "updated",
            "direction": "asc",
            "per_page": str(bounds.page_size),
            "page": str(page_number),
        }
        if stream_name in {"issue_comments", "review_comments"}:
            query.pop("state", None)
        if bounds.updated_since is not None and stream_name in {"issues", "issue_comments"}:
            query["since"] = _format_github_timestamp(bounds.updated_since)
        return urljoin(self._base_url, endpoint) + "?" + urlencode(query)

    def _request_json(self, url: str) -> tuple[Any, dict[str, str]]:
        endpoint = url if url.startswith("http") else urljoin(self._base_url, url.lstrip("/"))
        request = Request(
            endpoint,
            headers={
                "Accept": "application/vnd.github+json",
                "Authorization": f"Bearer {self._token}",
                "X-GitHub-Api-Version": GITHUB_API_VERSION,
                "User-Agent": "agentic-kb-task-403",
            },
            method="GET",
        )

        try:
            with urlopen(request, timeout=self._timeout_seconds) as response:
                body = response.read().decode("utf-8")
                headers = dict(response.headers.items())
        except HTTPError as error:
            detail = _read_http_error_detail(error)
            raise GithubApiError(
                f"GitHub request failed with HTTP {error.code} via {endpoint}: {detail}"
            ) from error
        except (TimeoutError, URLError, OSError) as error:
            raise GithubApiError(f"Unable to reach GitHub API at {endpoint}: {error}") from error

        try:
            payload = json.loads(body)
        except json.JSONDecodeError as error:
            raise GithubApiResponseError(
                f"GitHub API at {endpoint} returned invalid JSON: {error}"
            ) from error

        return payload, headers

    @staticmethod
    def _require_object(payload: Any, *, context: str) -> dict[str, Any]:
        if not isinstance(payload, dict):
            raise GithubApiResponseError(f"Expected JSON object for {context}, got {type(payload).__name__}")
        return payload


def deterministic_github_issue_id(repo: str, issue_number: int) -> str:
    return f"github-issue:{repo}#{issue_number}"


def deterministic_github_issue_comment_id(repo: str, github_comment_id: int) -> str:
    return f"github-issue-comment:{repo}#{github_comment_id}"


def deterministic_github_pr_id(repo: str, pr_number: int) -> str:
    return f"github-pr:{repo}#{pr_number}"


def deterministic_github_pr_comment_id(
    repo: str,
    *,
    comment_type: str,
    github_comment_id: int,
) -> str:
    return f"github-pr-comment:{repo}:{comment_type}#{github_comment_id}"


def build_github_preview_text(content: str, *, max_length: int = GITHUB_PREVIEW_LENGTH) -> str:
    return build_preview_text(content, max_length=max_length)


def iter_github_pages(
    *,
    bounds: GithubFetchBounds | None = None,
    embedding_client: EmbeddingClient,
    github_client: GithubApiClient,
    parent_cache: _GithubParentCache | None = None,
) -> Iterator[GithubPageBatch]:
    active_bounds = bounds or GithubFetchBounds()
    cache = parent_cache or _GithubParentCache(repo=active_bounds.repo)

    for stream_name in STREAM_ORDER:
        page_number = 1
        page_limit = active_bounds.page_limit_for_stream(stream_name)

        while True:
            payloads, has_next_page = github_client.fetch_stream_page(
                stream_name,
                bounds=active_bounds,
                page_number=page_number,
            )
            filtered_payloads = _filter_payloads_by_updated_since(
                payloads,
                updated_since=active_bounds.updated_since,
                stream_name=stream_name,
            )
            truncated_by_bound = bool(
                page_limit is not None and has_next_page and page_number >= page_limit
            )
            batch = _prepare_page_batch(
                stream_name,
                page_number=page_number,
                repo=active_bounds.repo,
                payloads=filtered_payloads,
                embedding_client=embedding_client,
                github_client=github_client,
                parent_cache=cache,
                has_next_page=has_next_page,
                truncated_by_bound=truncated_by_bound,
            )
            yield batch

            if truncated_by_bound or not has_next_page:
                break
            page_number += 1


def write_github_page_batch(
    batch: GithubPageBatch,
    *,
    github_store: GithubStore,
) -> GithubPageWriteResult:
    issues_written = github_store.upsert_issues(batch.issues)
    prs_written = github_store.upsert_prs(batch.pull_requests)
    issue_comments_written = github_store.upsert_issue_comments(batch.issue_comments)
    pr_comments_written = github_store.upsert_pr_comments(batch.pr_comments)

    return GithubPageWriteResult(
        stream_name=batch.stream_name,
        page_number=batch.page_number,
        issues_written=issues_written,
        issue_comments_written=issue_comments_written,
        prs_written=prs_written,
        pr_comments_written=pr_comments_written,
        latest_source_updated_at=batch.latest_source_updated_at,
    )


def ingest_github(
    *,
    embedding_client: EmbeddingClient,
    github_store: GithubStore,
    bounds: GithubFetchBounds | None = None,
    github_token: str | None = None,
    github_client: GithubApiClient | None = None,
    request_timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
) -> GithubIngestResult:
    active_bounds = bounds or GithubFetchBounds()
    client = github_client or GithubApiClient(
        token=github_token or "",
        timeout_seconds=request_timeout_seconds,
    )
    parent_cache = _GithubParentCache(repo=active_bounds.repo)
    progress = {
        stream_name: _MutableStreamProgress(stream_name=stream_name)
        for stream_name in STREAM_ORDER
    }

    for batch in iter_github_pages(
        bounds=active_bounds,
        embedding_client=embedding_client,
        github_client=client,
        parent_cache=parent_cache,
    ):
        write_result = write_github_page_batch(batch, github_store=github_store)
        progress[batch.stream_name].observe_batch(batch, write_result)

    frozen_progress = {
        stream_name: stream_progress.freeze()
        for stream_name, stream_progress in progress.items()
    }

    return GithubIngestResult(
        repo=active_bounds.repo,
        bounds=active_bounds,
        stream_progress=frozen_progress,
        issues_written=sum(item.issues_written for item in frozen_progress.values()),
        issue_comments_written=sum(
            item.issue_comments_written for item in frozen_progress.values()
        ),
        prs_written=sum(item.prs_written for item in frozen_progress.values()),
        pr_comments_written=sum(item.pr_comments_written for item in frozen_progress.values()),
    )


def ingest_github_from_config(
    *,
    config: AgenticConfig | None = None,
    bounds: GithubFetchBounds | None = None,
    request_timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
) -> GithubIngestResult:
    active_config = config or AgenticConfig.from_env()
    if not active_config.database_url:
        raise ValueError("DATABASE_URL is required for GitHub ingestion")
    if not active_config.github_token:
        raise MissingGithubTokenError("GITHUB_TOKEN is required for GitHub ingestion")

    embedding_client = OllamaEmbeddingClient.from_config(active_config)
    active_bounds = bounds or GithubFetchBounds()
    attempted_at = datetime.now(timezone.utc)
    with PostgresGithubStore.from_database_url(active_config.database_url) as github_store:
        with PostgresSyncStateStore.from_database_url(active_config.database_url) as sync_store:
            sync_store.record_attempts(
                [
                    build_sync_attempt(
                        "github",
                        github_scope_key(stream_name, repo=active_bounds.repo),
                        attempted_at=attempted_at,
                        metadata={"repo": active_bounds.repo, "stream_name": stream_name},
                    )
                    for stream_name in STREAM_ORDER
                ]
            )
            try:
                result = ingest_github(
                    embedding_client=embedding_client,
                    github_store=github_store,
                    bounds=active_bounds,
                    github_token=active_config.github_token,
                    request_timeout_seconds=request_timeout_seconds,
                )
            except Exception as error:
                sync_store.record_failures(
                    [
                        build_sync_failure(
                            "github",
                            github_scope_key(stream_name, repo=active_bounds.repo),
                            attempted_at=attempted_at,
                            error=error,
                            metadata={"repo": active_bounds.repo, "stream_name": stream_name},
                        )
                        for stream_name in STREAM_ORDER
                    ]
                )
                raise

            sync_store.upsert_sync_states(
                build_github_sync_state_updates(
                    result,
                    attempted_at=attempted_at,
                    succeeded_at=datetime.now(timezone.utc),
                )
            )
            return result


def _prepare_page_batch(
    stream_name: str,
    *,
    page_number: int,
    repo: str,
    payloads: Sequence[dict[str, Any]],
    embedding_client: EmbeddingClient,
    github_client: GithubApiClient,
    parent_cache: _GithubParentCache,
    has_next_page: bool,
    truncated_by_bound: bool,
) -> GithubPageBatch:
    issues: list[PreparedGithubIssue] = []
    issue_comments: list[PreparedGithubIssueComment] = []
    pull_requests: list[PreparedGithubPullRequest] = []
    pr_comments: list[PreparedGithubPrComment] = []
    latest_source_updated_at: datetime | None = None

    if stream_name == "issues":
        for payload in payloads:
            if "pull_request" in payload:
                continue
            issue = _prepare_issue(payload, repo=repo, embedding_client=embedding_client)
            parent_cache.issues_by_number[issue.issue_number] = issue
            issues.append(issue)
            latest_source_updated_at = _max_datetime(latest_source_updated_at, issue.source_updated_at)
    elif stream_name == "pulls":
        for payload in payloads:
            pull_request = _prepare_pull_request(
                payload,
                repo=repo,
                embedding_client=embedding_client,
            )
            parent_cache.prs_by_number[pull_request.pr_number] = pull_request
            pull_requests.append(pull_request)
            latest_source_updated_at = _max_datetime(
                latest_source_updated_at,
                pull_request.source_updated_at,
            )
    elif stream_name == "issue_comments":
        for payload in payloads:
            parent_kind, parent_issue, parent_pr = _resolve_issue_comment_parent(
                payload,
                repo=repo,
                embedding_client=embedding_client,
                github_client=github_client,
                parent_cache=parent_cache,
            )
            if parent_issue is not None and parent_issue.issue_number not in parent_cache.issues_by_number:
                parent_cache.issues_by_number[parent_issue.issue_number] = parent_issue
                issues.append(parent_issue)
            if parent_pr is not None and parent_pr.pr_number not in parent_cache.prs_by_number:
                parent_cache.prs_by_number[parent_pr.pr_number] = parent_pr
                pull_requests.append(parent_pr)

            if parent_kind == "issue" and parent_issue is not None:
                comment = _prepare_issue_comment(
                    payload,
                    repo=repo,
                    parent_issue=parent_issue,
                    embedding_client=embedding_client,
                )
                issue_comments.append(comment)
                latest_source_updated_at = _max_datetime(
                    latest_source_updated_at,
                    comment.source_updated_at,
                )
                continue

            if parent_kind == "pr" and parent_pr is not None:
                comment = _prepare_pr_issue_comment(
                    payload,
                    repo=repo,
                    parent_pr=parent_pr,
                    embedding_client=embedding_client,
                )
                pr_comments.append(comment)
                latest_source_updated_at = _max_datetime(
                    latest_source_updated_at,
                    comment.source_updated_at,
                )
                continue

            raise GithubParentRoutingError("Unable to route GitHub issue comment to issue or pull request")
    elif stream_name == "review_comments":
        for payload in payloads:
            parent_pr, was_cached = _resolve_review_comment_parent(
                payload,
                repo=repo,
                embedding_client=embedding_client,
                github_client=github_client,
                parent_cache=parent_cache,
            )
            if not was_cached:
                parent_cache.prs_by_number[parent_pr.pr_number] = parent_pr
                pull_requests.append(parent_pr)
            comment = _prepare_review_comment(
                payload,
                repo=repo,
                parent_pr=parent_pr,
                embedding_client=embedding_client,
            )
            pr_comments.append(comment)
            latest_source_updated_at = _max_datetime(
                latest_source_updated_at,
                comment.source_updated_at,
            )
    else:
        raise GithubPaginationError(f"Unknown GitHub stream {stream_name!r}")

    if stream_name in {"issues", "pulls"}:
        latest_source_updated_at = _max_datetime(
            latest_source_updated_at,
            *(item.source_updated_at for item in issues),
            *(item.source_updated_at for item in pull_requests),
        )
    return GithubPageBatch(
        stream_name=stream_name,
        page_number=page_number,
        has_next_page=has_next_page,
        truncated_by_bound=truncated_by_bound,
        latest_source_updated_at=latest_source_updated_at,
        issues=tuple(issues),
        issue_comments=tuple(issue_comments),
        pull_requests=tuple(pull_requests),
        pr_comments=tuple(pr_comments),
    )


def _prepare_issue(
    payload: dict[str, Any],
    *,
    repo: str,
    embedding_client: EmbeddingClient,
) -> PreparedGithubIssue:
    issue_number = _require_int(payload.get("number"), context="issue number")
    title = _normalize_title(payload.get("title"), fallback=f"Issue #{issue_number}")
    raw_body = _normalize_text(payload.get("body"))
    body_text, body_metadata = _normalize_parent_body_text(title=title, raw_body=raw_body)

    return PreparedGithubIssue(
        id=deterministic_github_issue_id(repo, issue_number),
        repo=repo,
        issue_number=issue_number,
        github_node_id=_string_or_none(payload.get("node_id")),
        title=title,
        state=_normalize_title(payload.get("state"), fallback="unknown").lower(),
        author_login=_user_login(payload.get("user")),
        labels=_extract_labels(payload.get("labels")),
        body_text=body_text,
        preview_text=build_github_preview_text(body_text),
        html_url=_normalize_title(payload.get("html_url"), fallback=f"https://github.com/{repo}/issues/{issue_number}"),
        source_created_at=_parse_timestamp(payload.get("created_at"), context="issue created_at"),
        source_updated_at=_parse_timestamp(payload.get("updated_at"), context="issue updated_at"),
        source_closed_at=_parse_optional_timestamp(payload.get("closed_at")),
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata={
            **body_metadata,
            "author_association": _string_or_none(payload.get("author_association")),
            "assignees": _extract_logins(payload.get("assignees")),
            "milestone": _milestone_title(payload.get("milestone")),
            "comments_count": _require_int(payload.get("comments"), context="issue comments", default=0),
            "state_reason": _string_or_none(payload.get("state_reason")),
            "locked": bool(payload.get("locked", False)),
        },
    )


def _prepare_pull_request(
    payload: dict[str, Any],
    *,
    repo: str,
    embedding_client: EmbeddingClient,
) -> PreparedGithubPullRequest:
    pr_number = _require_int(payload.get("number"), context="pull request number")
    title = _normalize_title(payload.get("title"), fallback=f"Pull Request #{pr_number}")
    raw_body = _normalize_text(payload.get("body"))
    body_text, body_metadata = _normalize_parent_body_text(title=title, raw_body=raw_body)
    base = _require_object(payload.get("base"), context="pull request base")
    head = _require_object(payload.get("head"), context="pull request head")

    return PreparedGithubPullRequest(
        id=deterministic_github_pr_id(repo, pr_number),
        repo=repo,
        pr_number=pr_number,
        github_node_id=_string_or_none(payload.get("node_id")),
        title=title,
        state=_normalize_title(payload.get("state"), fallback="unknown").lower(),
        author_login=_user_login(payload.get("user")),
        base_branch=_string_or_none(base.get("ref")),
        head_branch=_string_or_none(head.get("ref")),
        labels=_extract_labels(payload.get("labels")),
        body_text=body_text,
        preview_text=build_github_preview_text(body_text),
        html_url=_normalize_title(payload.get("html_url"), fallback=f"https://github.com/{repo}/pull/{pr_number}"),
        source_created_at=_parse_timestamp(payload.get("created_at"), context="pull request created_at"),
        source_updated_at=_parse_timestamp(payload.get("updated_at"), context="pull request updated_at"),
        source_closed_at=_parse_optional_timestamp(payload.get("closed_at")),
        source_merged_at=_parse_optional_timestamp(payload.get("merged_at")),
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata={
            **body_metadata,
            "draft": bool(payload.get("draft", False)),
            "merged": bool(payload.get("merged_at")),
            "merge_commit_sha": _string_or_none(payload.get("merge_commit_sha")),
            "requested_reviewers": _extract_logins(payload.get("requested_reviewers")),
            "review_comments_count": _require_int(
                payload.get("review_comments"),
                context="pull request review_comments",
                default=0,
            ),
            "commits": _require_int(payload.get("commits"), context="pull request commits", default=0),
            "changed_files": _require_int(
                payload.get("changed_files"),
                context="pull request changed_files",
                default=0,
            ),
        },
    )


def _prepare_issue_comment(
    payload: dict[str, Any],
    *,
    repo: str,
    parent_issue: PreparedGithubIssue,
    embedding_client: EmbeddingClient,
) -> PreparedGithubIssueComment:
    body_text, metadata = _normalize_comment_body_text(payload)
    github_comment_id = _require_int(payload.get("id"), context="issue comment id")
    return PreparedGithubIssueComment(
        id=deterministic_github_issue_comment_id(repo, github_comment_id),
        issue_id=parent_issue.id,
        repo=repo,
        issue_number=parent_issue.issue_number,
        github_comment_id=github_comment_id,
        github_node_id=_string_or_none(payload.get("node_id")),
        author_login=_user_login(payload.get("user")),
        body_text=body_text,
        preview_text=build_github_preview_text(body_text),
        html_url=_normalize_title(payload.get("html_url"), fallback=parent_issue.html_url),
        source_created_at=_parse_timestamp(payload.get("created_at"), context="issue comment created_at"),
        source_updated_at=_parse_timestamp(payload.get("updated_at"), context="issue comment updated_at"),
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata=metadata,
    )


def _prepare_pr_issue_comment(
    payload: dict[str, Any],
    *,
    repo: str,
    parent_pr: PreparedGithubPullRequest,
    embedding_client: EmbeddingClient,
) -> PreparedGithubPrComment:
    body_text, metadata = _normalize_comment_body_text(payload)
    github_comment_id = _require_int(payload.get("id"), context="PR discussion comment id")
    return PreparedGithubPrComment(
        id=deterministic_github_pr_comment_id(
            repo,
            comment_type="issue_comment",
            github_comment_id=github_comment_id,
        ),
        pr_id=parent_pr.id,
        repo=repo,
        pr_number=parent_pr.pr_number,
        comment_type="issue_comment",
        github_comment_id=github_comment_id,
        github_node_id=_string_or_none(payload.get("node_id")),
        author_login=_user_login(payload.get("user")),
        repo_path=None,
        commit_oid=None,
        original_commit_oid=None,
        diff_hunk=None,
        line=None,
        original_line=None,
        side=None,
        start_line=None,
        start_side=None,
        body_text=body_text,
        preview_text=build_github_preview_text(body_text),
        html_url=_normalize_title(payload.get("html_url"), fallback=parent_pr.html_url),
        source_created_at=_parse_timestamp(payload.get("created_at"), context="PR discussion comment created_at"),
        source_updated_at=_parse_timestamp(payload.get("updated_at"), context="PR discussion comment updated_at"),
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata=metadata,
    )


def _prepare_review_comment(
    payload: dict[str, Any],
    *,
    repo: str,
    parent_pr: PreparedGithubPullRequest,
    embedding_client: EmbeddingClient,
) -> PreparedGithubPrComment:
    body_text, metadata = _normalize_comment_body_text(payload)
    github_comment_id = _require_int(payload.get("id"), context="review comment id")
    metadata.update(
        {
            "in_reply_to_id": _require_int(payload.get("in_reply_to_id"), context="in_reply_to_id", default=None),
            "pull_request_review_id": _require_int(
                payload.get("pull_request_review_id"),
                context="pull_request_review_id",
                default=None,
            ),
            "position": _require_int(payload.get("position"), context="review comment position", default=None),
            "subject_type": _string_or_none(payload.get("subject_type")),
        }
    )
    return PreparedGithubPrComment(
        id=deterministic_github_pr_comment_id(
            repo,
            comment_type="review_comment",
            github_comment_id=github_comment_id,
        ),
        pr_id=parent_pr.id,
        repo=repo,
        pr_number=parent_pr.pr_number,
        comment_type="review_comment",
        github_comment_id=github_comment_id,
        github_node_id=_string_or_none(payload.get("node_id")),
        author_login=_user_login(payload.get("user")),
        repo_path=_string_or_none(payload.get("path")),
        commit_oid=_string_or_none(payload.get("commit_id")),
        original_commit_oid=_string_or_none(payload.get("original_commit_id")),
        diff_hunk=_string_or_none(payload.get("diff_hunk")),
        line=_require_int(payload.get("line"), context="review comment line", default=None),
        original_line=_require_int(
            payload.get("original_line"),
            context="review comment original_line",
            default=None,
        ),
        side=_string_or_none(payload.get("side")),
        start_line=_require_int(
            payload.get("start_line"),
            context="review comment start_line",
            default=None,
        ),
        start_side=_string_or_none(payload.get("start_side")),
        body_text=body_text,
        preview_text=build_github_preview_text(body_text),
        html_url=_normalize_title(payload.get("html_url"), fallback=parent_pr.html_url),
        source_created_at=_parse_timestamp(payload.get("created_at"), context="review comment created_at"),
        source_updated_at=_parse_timestamp(payload.get("updated_at"), context="review comment updated_at"),
        embedding=_embed_document_content(body_text, embedding_client=embedding_client),
        metadata=metadata,
    )


def _resolve_issue_comment_parent(
    payload: dict[str, Any],
    *,
    repo: str,
    embedding_client: EmbeddingClient,
    github_client: GithubApiClient,
    parent_cache: _GithubParentCache,
) -> tuple[str, PreparedGithubIssue | None, PreparedGithubPullRequest | None]:
    issue_url = _normalize_title(payload.get("issue_url"), fallback="")
    issue_number = _extract_number(issue_url, ISSUE_NUMBER_PATTERN, label="issue comment issue_url")

    if issue_number in parent_cache.issues_by_number:
        return "issue", parent_cache.issues_by_number[issue_number], None
    if issue_number in parent_cache.prs_by_number:
        return "pr", None, parent_cache.prs_by_number[issue_number]

    issue_payload = github_client.fetch_issue_payload(issue_url)
    if "pull_request" in issue_payload:
        pull_request = _prepare_pull_request(
            github_client.fetch_pull_request(repo, issue_number),
            repo=repo,
            embedding_client=embedding_client,
        )
        return "pr", None, pull_request

    issue = _prepare_issue(issue_payload, repo=repo, embedding_client=embedding_client)
    return "issue", issue, None


def _resolve_review_comment_parent(
    payload: dict[str, Any],
    *,
    repo: str,
    embedding_client: EmbeddingClient,
    github_client: GithubApiClient,
    parent_cache: _GithubParentCache,
) -> tuple[PreparedGithubPullRequest, bool]:
    pr_url = _normalize_title(payload.get("pull_request_url"), fallback="")
    pr_number = _extract_number(pr_url, PULL_NUMBER_PATTERN, label="review comment pull_request_url")
    cached = parent_cache.prs_by_number.get(pr_number)
    if cached is not None:
        return cached, True

    pull_request = _prepare_pull_request(
        github_client.fetch_pull_request(repo, pr_number),
        repo=repo,
        embedding_client=embedding_client,
    )
    return pull_request, False


def _filter_payloads_by_updated_since(
    payloads: Sequence[dict[str, Any]],
    *,
    updated_since: datetime | None,
    stream_name: str,
) -> list[dict[str, Any]]:
    if updated_since is None:
        return list(payloads)
    if stream_name in {"issues", "issue_comments"}:
        return list(payloads)

    filtered: list[dict[str, Any]] = []
    for payload in payloads:
        updated_at = _parse_timestamp(payload.get("updated_at"), context="updated_at")
        if updated_at >= updated_since:
            filtered.append(payload)
    return filtered


def _format_github_timestamp(value: datetime) -> str:
    normalized = value.astimezone(timezone.utc)
    return normalized.replace(microsecond=0).isoformat().replace("+00:00", "Z")


def _normalize_parent_body_text(*, title: str, raw_body: str) -> tuple[str, dict[str, Any]]:
    if raw_body:
        return f"{title}\n\n{raw_body}", {"body_was_empty": False, "body_text_source": "title_plus_body"}
    return title, {"body_was_empty": True, "body_text_source": "title_only"}


def _normalize_comment_body_text(payload: dict[str, Any]) -> tuple[str, dict[str, Any]]:
    raw_body = _normalize_text(payload.get("body"))
    if raw_body:
        return raw_body, {"body_was_empty": False}
    return "[empty GitHub comment]", {"body_was_empty": True}


def _normalize_text(value: Any) -> str:
    if value is None:
        return ""
    if not isinstance(value, str):
        raise GithubApiResponseError(f"Expected text field, got {type(value).__name__}")
    return _normalize_content(value).strip()


def _normalize_title(value: Any, *, fallback: str) -> str:
    normalized = _normalize_text(value)
    return normalized or fallback


def _parse_timestamp(value: Any, *, context: str) -> datetime:
    if not isinstance(value, str) or not value:
        raise GithubApiResponseError(f"Missing or invalid timestamp for {context}")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise GithubApiResponseError(f"Invalid timestamp for {context}: {value!r}") from error
    if parsed.tzinfo is None:
        return parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def _parse_optional_timestamp(value: Any) -> datetime | None:
    if value is None:
        return None
    return _parse_timestamp(value, context="optional timestamp")


def _extract_labels(value: Any) -> list[str]:
    if value is None:
        return []
    if not isinstance(value, list):
        raise GithubApiResponseError("Expected labels to be a list")
    labels: list[str] = []
    for item in value:
        if isinstance(item, dict):
            label_name = _string_or_none(item.get("name"))
            if label_name:
                labels.append(label_name)
    return labels


def _extract_logins(value: Any) -> list[str]:
    if value is None:
        return []
    if not isinstance(value, list):
        raise GithubApiResponseError("Expected user list to be a list")
    logins: list[str] = []
    for item in value:
        login = _user_login(item)
        if login:
            logins.append(login)
    return logins


def _user_login(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, dict):
        raise GithubApiResponseError("Expected user payload to be an object")
    return _string_or_none(value.get("login"))


def _milestone_title(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, dict):
        raise GithubApiResponseError("Expected milestone payload to be an object")
    return _string_or_none(value.get("title"))


def _string_or_none(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, str):
        raise GithubApiResponseError(f"Expected string field, got {type(value).__name__}")
    normalized = _normalize_content(value).strip()
    return normalized or None


def _require_object(value: Any, *, context: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise GithubApiResponseError(f"Expected object for {context}")
    return value


def _require_int(value: Any, *, context: str, default: int | None = None) -> int | None:
    if value is None:
        return default
    if isinstance(value, bool) or not isinstance(value, int):
        raise GithubApiResponseError(f"Expected integer for {context}")
    return value


def _extract_number(url: str, pattern: re.Pattern[str], *, label: str) -> int:
    match = pattern.search(url)
    if match is None:
        raise GithubParentRoutingError(f"Unable to parse numeric identifier from {label}: {url!r}")
    return int(match.group(1))


def _parse_next_link(link_header: str | None) -> str | None:
    if not link_header:
        return None
    links = [segment.strip() for segment in link_header.split(",") if segment.strip()]
    next_links: list[str] = []
    for link in links:
        parts = [part.strip() for part in link.split(";")]
        if not parts:
            continue
        url_part = parts[0]
        if not (url_part.startswith("<") and url_part.endswith(">")):
            raise GithubPaginationError(f"Malformed GitHub Link header segment: {link!r}")
        rels = {
            part.split("=", 1)[1].strip('"')
            for part in parts[1:]
            if part.startswith("rel=") and "=" in part
        }
        if "next" in rels:
            next_links.append(url_part[1:-1])

    if len(next_links) > 1:
        raise GithubPaginationError("GitHub Link header contained multiple next relations")
    if not next_links:
        return None

    next_url = next_links[0]
    parsed = urlparse(next_url)
    page_values = parse_qs(parsed.query).get("page")
    if page_values and len(page_values) != 1:
        raise GithubPaginationError(f"Unexpected next page contract in Link header: {next_url!r}")
    return next_url


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


class PostgresGithubStore:
    def __init__(self, connection: Any):
        self._connection = connection

    @classmethod
    def from_database_url(cls, database_url: str) -> "PostgresGithubStore":
        psycopg, _ = _load_psycopg()
        return cls(psycopg.connect(database_url))

    def __enter__(self) -> "PostgresGithubStore":
        return self

    def __exit__(self, exc_type, exc, tb) -> bool:
        self.close()
        return False

    def close(self) -> None:
        self._connection.close()

    def upsert_issues(self, issues: Sequence[PreparedGithubIssue]) -> int:
        if not issues:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_github_issues (
                        id,
                        repo,
                        issue_number,
                        github_node_id,
                        title,
                        state,
                        author_login,
                        labels,
                        body_text,
                        preview_text,
                        html_url,
                        source_created_at,
                        source_updated_at,
                        source_closed_at,
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
                    ON CONFLICT (repo, issue_number) DO UPDATE SET
                        id = EXCLUDED.id,
                        github_node_id = EXCLUDED.github_node_id,
                        title = EXCLUDED.title,
                        state = EXCLUDED.state,
                        author_login = EXCLUDED.author_login,
                        labels = EXCLUDED.labels,
                        body_text = EXCLUDED.body_text,
                        preview_text = EXCLUDED.preview_text,
                        html_url = EXCLUDED.html_url,
                        source_created_at = EXCLUDED.source_created_at,
                        source_updated_at = EXCLUDED.source_updated_at,
                        source_closed_at = EXCLUDED.source_closed_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            issue.id,
                            issue.repo,
                            issue.issue_number,
                            issue.github_node_id,
                            issue.title,
                            issue.state,
                            issue.author_login,
                            Json(issue.labels),
                            issue.body_text,
                            issue.preview_text,
                            issue.html_url,
                            issue.source_created_at,
                            issue.source_updated_at,
                            issue.source_closed_at,
                            _vector_literal(issue.embedding),
                            Json(issue.metadata),
                        )
                        for issue in issues
                    ],
                )
        return len(issues)

    def upsert_prs(self, pull_requests: Sequence[PreparedGithubPullRequest]) -> int:
        if not pull_requests:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_github_prs (
                        id,
                        repo,
                        pr_number,
                        github_node_id,
                        title,
                        state,
                        author_login,
                        base_branch,
                        head_branch,
                        labels,
                        body_text,
                        preview_text,
                        html_url,
                        source_created_at,
                        source_updated_at,
                        source_closed_at,
                        source_merged_at,
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
                        %s::vector,
                        %s,
                        NOW()
                    )
                    ON CONFLICT (repo, pr_number) DO UPDATE SET
                        id = EXCLUDED.id,
                        github_node_id = EXCLUDED.github_node_id,
                        title = EXCLUDED.title,
                        state = EXCLUDED.state,
                        author_login = EXCLUDED.author_login,
                        base_branch = EXCLUDED.base_branch,
                        head_branch = EXCLUDED.head_branch,
                        labels = EXCLUDED.labels,
                        body_text = EXCLUDED.body_text,
                        preview_text = EXCLUDED.preview_text,
                        html_url = EXCLUDED.html_url,
                        source_created_at = EXCLUDED.source_created_at,
                        source_updated_at = EXCLUDED.source_updated_at,
                        source_closed_at = EXCLUDED.source_closed_at,
                        source_merged_at = EXCLUDED.source_merged_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            pull_request.id,
                            pull_request.repo,
                            pull_request.pr_number,
                            pull_request.github_node_id,
                            pull_request.title,
                            pull_request.state,
                            pull_request.author_login,
                            pull_request.base_branch,
                            pull_request.head_branch,
                            Json(pull_request.labels),
                            pull_request.body_text,
                            pull_request.preview_text,
                            pull_request.html_url,
                            pull_request.source_created_at,
                            pull_request.source_updated_at,
                            pull_request.source_closed_at,
                            pull_request.source_merged_at,
                            _vector_literal(pull_request.embedding),
                            Json(pull_request.metadata),
                        )
                        for pull_request in pull_requests
                    ],
                )
        return len(pull_requests)

    def upsert_issue_comments(
        self,
        comments: Sequence[PreparedGithubIssueComment],
    ) -> int:
        if not comments:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_github_issue_comments (
                        id,
                        issue_id,
                        repo,
                        issue_number,
                        github_comment_id,
                        github_node_id,
                        author_login,
                        body_text,
                        preview_text,
                        html_url,
                        source_created_at,
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
                        %s::vector,
                        %s,
                        NOW()
                    )
                    ON CONFLICT (repo, github_comment_id) DO UPDATE SET
                        id = EXCLUDED.id,
                        issue_id = EXCLUDED.issue_id,
                        issue_number = EXCLUDED.issue_number,
                        github_node_id = EXCLUDED.github_node_id,
                        author_login = EXCLUDED.author_login,
                        body_text = EXCLUDED.body_text,
                        preview_text = EXCLUDED.preview_text,
                        html_url = EXCLUDED.html_url,
                        source_created_at = EXCLUDED.source_created_at,
                        source_updated_at = EXCLUDED.source_updated_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            comment.id,
                            comment.issue_id,
                            comment.repo,
                            comment.issue_number,
                            comment.github_comment_id,
                            comment.github_node_id,
                            comment.author_login,
                            comment.body_text,
                            comment.preview_text,
                            comment.html_url,
                            comment.source_created_at,
                            comment.source_updated_at,
                            _vector_literal(comment.embedding),
                            Json(comment.metadata),
                        )
                        for comment in comments
                    ],
                )
        return len(comments)

    def upsert_pr_comments(self, comments: Sequence[PreparedGithubPrComment]) -> int:
        if not comments:
            return 0
        _, Json = _load_psycopg()
        with self._connection.transaction():
            with self._connection.cursor() as cursor:
                cursor.executemany(
                    """
                    INSERT INTO agentic.kb_github_pr_comments (
                        id,
                        pr_id,
                        repo,
                        pr_number,
                        comment_type,
                        github_comment_id,
                        github_node_id,
                        author_login,
                        repo_path,
                        commit_oid,
                        original_commit_oid,
                        diff_hunk,
                        line,
                        original_line,
                        side,
                        start_line,
                        start_side,
                        body_text,
                        preview_text,
                        html_url,
                        source_created_at,
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
                    ON CONFLICT (repo, comment_type, github_comment_id) DO UPDATE SET
                        id = EXCLUDED.id,
                        pr_id = EXCLUDED.pr_id,
                        pr_number = EXCLUDED.pr_number,
                        github_node_id = EXCLUDED.github_node_id,
                        author_login = EXCLUDED.author_login,
                        repo_path = EXCLUDED.repo_path,
                        commit_oid = EXCLUDED.commit_oid,
                        original_commit_oid = EXCLUDED.original_commit_oid,
                        diff_hunk = EXCLUDED.diff_hunk,
                        line = EXCLUDED.line,
                        original_line = EXCLUDED.original_line,
                        side = EXCLUDED.side,
                        start_line = EXCLUDED.start_line,
                        start_side = EXCLUDED.start_side,
                        body_text = EXCLUDED.body_text,
                        preview_text = EXCLUDED.preview_text,
                        html_url = EXCLUDED.html_url,
                        source_created_at = EXCLUDED.source_created_at,
                        source_updated_at = EXCLUDED.source_updated_at,
                        embedding = EXCLUDED.embedding,
                        metadata = EXCLUDED.metadata,
                        updated_at = NOW()
                    """,
                    [
                        (
                            comment.id,
                            comment.pr_id,
                            comment.repo,
                            comment.pr_number,
                            comment.comment_type,
                            comment.github_comment_id,
                            comment.github_node_id,
                            comment.author_login,
                            comment.repo_path,
                            comment.commit_oid,
                            comment.original_commit_oid,
                            comment.diff_hunk,
                            comment.line,
                            comment.original_line,
                            comment.side,
                            comment.start_line,
                            comment.start_side,
                            comment.body_text,
                            comment.preview_text,
                            comment.html_url,
                            comment.source_created_at,
                            comment.source_updated_at,
                            _vector_literal(comment.embedding),
                            Json(comment.metadata),
                        )
                        for comment in comments
                    ],
                )
        return len(comments)


class InMemoryGithubStore:
    def __init__(self):
        self.issues_by_key: dict[tuple[str, int], dict[str, Any]] = {}
        self.issue_comments_by_key: dict[tuple[str, int], dict[str, Any]] = {}
        self.prs_by_key: dict[tuple[str, int], dict[str, Any]] = {}
        self.pr_comments_by_key: dict[tuple[str, str, int], dict[str, Any]] = {}
        self.write_count = 0

    def upsert_issues(self, issues: Sequence[PreparedGithubIssue]) -> int:
        for issue in issues:
            self.write_count += 1
            self.issues_by_key[(issue.repo, issue.issue_number)] = {
                **issue.__dict__,
                "labels": list(issue.labels),
                "embedding": list(issue.embedding),
                "metadata": dict(issue.metadata),
                "updated_at_token": self.write_count,
            }
        return len(issues)

    def upsert_issue_comments(
        self,
        comments: Sequence[PreparedGithubIssueComment],
    ) -> int:
        for comment in comments:
            self.write_count += 1
            self.issue_comments_by_key[(comment.repo, comment.github_comment_id)] = {
                **comment.__dict__,
                "embedding": list(comment.embedding),
                "metadata": dict(comment.metadata),
                "updated_at_token": self.write_count,
            }
        return len(comments)

    def upsert_prs(self, pull_requests: Sequence[PreparedGithubPullRequest]) -> int:
        for pull_request in pull_requests:
            self.write_count += 1
            self.prs_by_key[(pull_request.repo, pull_request.pr_number)] = {
                **pull_request.__dict__,
                "labels": list(pull_request.labels),
                "embedding": list(pull_request.embedding),
                "metadata": dict(pull_request.metadata),
                "updated_at_token": self.write_count,
            }
        return len(pull_requests)

    def upsert_pr_comments(self, comments: Sequence[PreparedGithubPrComment]) -> int:
        for comment in comments:
            self.write_count += 1
            self.pr_comments_by_key[
                (comment.repo, comment.comment_type, comment.github_comment_id)
            ] = {
                **comment.__dict__,
                "embedding": list(comment.embedding),
                "metadata": dict(comment.metadata),
                "updated_at_token": self.write_count,
            }
        return len(comments)
