from __future__ import annotations

import unittest
from datetime import datetime, timezone
from urllib.error import HTTPError
from urllib.parse import parse_qs, urlparse

from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import github


class FakeEmbeddingClient:
    def __init__(self):
        self.calls: list[list[str]] = []

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        self.calls.append(list(texts))
        return [[float(index)] * 384 for index, _ in enumerate(texts, start=1)]


class FakeGithubApiClient:
    def __init__(self, *, pages: dict[tuple[str, int], tuple[list[dict], bool]]):
        self.pages = pages
        self.issue_payloads: dict[str, dict] = {}
        self.pull_payloads: dict[tuple[str, int], dict] = {}
        self.page_calls: list[tuple[str, int]] = []
        self.page_bounds: list[tuple[str, github.GithubFetchBounds]] = []
        self.issue_calls: list[str] = []
        self.pull_calls: list[tuple[str, int]] = []

    def fetch_stream_page(
        self,
        stream_name: str,
        *,
        bounds: github.GithubFetchBounds,
        page_number: int,
    ) -> tuple[list[dict], bool]:
        self.page_calls.append((stream_name, page_number))
        self.page_bounds.append((stream_name, bounds))
        try:
            payloads, has_next = self.pages[(stream_name, page_number)]
        except KeyError as error:
            raise AssertionError(f"Missing fake page for {(stream_name, page_number)!r}") from error
        return payloads, has_next

    def fetch_issue_payload(self, issue_url: str) -> dict:
        self.issue_calls.append(issue_url)
        return self.issue_payloads[issue_url]

    def fetch_pull_request(self, repo: str, pr_number: int) -> dict:
        self.pull_calls.append((repo, pr_number))
        return self.pull_payloads[(repo, pr_number)]


class GithubIngestTests(unittest.TestCase):
    def test_iter_github_pages_filters_pr_stubs_from_issue_stream(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7), self._issue_stub_for_pr(8)], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        batches = list(
            github.iter_github_pages(
                bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
                embedding_client=embedding_client,
                github_client=client,
            )
        )

        issue_batch = batches[0]
        self.assertEqual(issue_batch.stream_name, "issues")
        self.assertEqual([issue.issue_number for issue in issue_batch.issues], [7])
        self.assertEqual(issue_batch.pull_requests, ())

    def test_iter_github_pages_maps_pull_request_parents_from_pulls_stream(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([], False),
                ("pulls", 1): ([self._pull_payload(11, title="Add GitHub ingest", body="PR body")], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        batches = list(
            github.iter_github_pages(
                bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
                embedding_client=embedding_client,
                github_client=client,
            )
        )

        pull_batch = next(batch for batch in batches if batch.stream_name == "pulls")
        self.assertEqual(len(pull_batch.pull_requests), 1)
        pull_request = pull_batch.pull_requests[0]
        self.assertEqual(pull_request.pr_number, 11)
        self.assertEqual(pull_request.base_branch, "main")
        self.assertEqual(pull_request.head_branch, "feature/task-403")
        self.assertEqual(pull_request.metadata["merged"], False)

    def test_issue_comments_route_to_issue_and_pr_comment_tables(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7)], False),
                ("pulls", 1): ([self._pull_payload(11)], False),
                (
                    "issue_comments",
                    1,
                ): (
                    [
                        self._issue_comment_payload(comment_id=1001, issue_number=7),
                        self._issue_comment_payload(comment_id=1002, issue_number=11),
                    ],
                    False,
                ),
                ("review_comments", 1): ([], False),
            }
        )

        store = github.InMemoryGithubStore()
        result = github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=client,
        )

        self.assertEqual(len(store.issue_comments_by_key), 1)
        self.assertEqual(len(store.pr_comments_by_key), 1)
        self.assertIn(("DripDropz/daedalus", 1001), store.issue_comments_by_key)
        self.assertIn(("DripDropz/daedalus", "issue_comment", 1002), store.pr_comments_by_key)
        self.assertEqual(result.issue_comments_written, 1)
        self.assertEqual(result.pr_comments_written, 1)

    def test_on_demand_parent_resolution_fetches_issue_and_canonical_pr_parent(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([], False),
                ("pulls", 1): ([], False),
                (
                    "issue_comments",
                    1,
                ): (
                    [
                        self._issue_comment_payload(comment_id=1001, issue_number=21),
                        self._issue_comment_payload(comment_id=1002, issue_number=22),
                    ],
                    False,
                ),
                ("review_comments", 1): ([], False),
            }
        )
        issue_url = "https://api.github.com/repos/DripDropz/daedalus/issues/21"
        pr_issue_url = "https://api.github.com/repos/DripDropz/daedalus/issues/22"
        client.issue_payloads[issue_url] = self._issue_payload(21, title="Fetched issue")
        client.issue_payloads[pr_issue_url] = self._issue_stub_for_pr(22)
        client.pull_payloads[("DripDropz/daedalus", 22)] = self._pull_payload(22, title="Fetched PR")

        batches = list(
            github.iter_github_pages(
                bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
                embedding_client=embedding_client,
                github_client=client,
            )
        )

        comments_batch = next(batch for batch in batches if batch.stream_name == "issue_comments")
        self.assertEqual([issue.issue_number for issue in comments_batch.issues], [21])
        self.assertEqual([pull.pr_number for pull in comments_batch.pull_requests], [22])
        self.assertEqual(len(comments_batch.issue_comments), 1)
        self.assertEqual(len(comments_batch.pr_comments), 1)
        self.assertEqual(client.issue_calls, [issue_url, pr_issue_url])
        self.assertEqual(client.pull_calls, [("DripDropz/daedalus", 22)])

    def test_comment_stream_progress_ignores_newer_side_loaded_parent_timestamps(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1002, issue_number=22, updated_at="2026-03-28T13:30:00Z")], False),
                ("review_comments", 1): ([], False),
            }
        )
        pr_issue_url = "https://api.github.com/repos/DripDropz/daedalus/issues/22"
        client.issue_payloads[pr_issue_url] = self._issue_stub_for_pr(22)
        client.pull_payloads[("DripDropz/daedalus", 22)] = self._pull_payload(
            22,
            title="Fetched PR",
            updated_at="2026-03-29T00:00:00Z",
        )
        store = github.InMemoryGithubStore()

        result = github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=client,
        )

        self.assertEqual(result.stream_progress["issue_comments"].latest_source_updated_at, datetime(2026, 3, 28, 13, 30, tzinfo=timezone.utc))
        self.assertEqual(result.stream_progress["pulls"].latest_source_updated_at, None)

    def test_review_comments_map_review_context_columns(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([], False),
                ("pulls", 1): ([self._pull_payload(11)], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([self._review_comment_payload(comment_id=2001, pr_number=11)], False),
            }
        )

        batches = list(
            github.iter_github_pages(
                bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
                embedding_client=embedding_client,
                github_client=client,
            )
        )

        review_batch = next(batch for batch in batches if batch.stream_name == "review_comments")
        self.assertEqual(len(review_batch.pr_comments), 1)
        review_comment = review_batch.pr_comments[0]
        self.assertEqual(review_comment.comment_type, "review_comment")
        self.assertEqual(review_comment.repo_path, "source/main/config.ts")
        self.assertEqual(review_comment.line, 42)
        self.assertEqual(review_comment.original_line, 40)
        self.assertEqual(review_comment.side, "RIGHT")
        self.assertEqual(review_comment.start_line, 41)
        self.assertEqual(review_comment.start_side, "RIGHT")
        self.assertEqual(review_comment.metadata["pull_request_review_id"], 9001)
        self.assertEqual(review_comment.metadata["in_reply_to_id"], 1999)

    def test_helper_contracts_cover_ids_preview_and_body_fallbacks(self):
        issue = github._prepare_issue(
            self._issue_payload(7, body=""),
            repo="DripDropz/daedalus",
            embedding_client=FakeEmbeddingClient(),
        )

        self.assertEqual(github.deterministic_github_issue_id("DripDropz/daedalus", 7), "github-issue:DripDropz/daedalus#7")
        self.assertEqual(
            github.deterministic_github_pr_id("DripDropz/daedalus", 11),
            "github-pr:DripDropz/daedalus#11",
        )
        self.assertEqual(
            github.deterministic_github_issue_comment_id("DripDropz/daedalus", 1001),
            "github-issue-comment:DripDropz/daedalus#1001",
        )
        self.assertEqual(
            github.deterministic_github_pr_comment_id(
                "DripDropz/daedalus",
                comment_type="review_comment",
                github_comment_id=2001,
            ),
            "github-pr-comment:DripDropz/daedalus:review_comment#2001",
        )
        self.assertEqual(issue.body_text, "Issue 7")
        self.assertTrue(issue.metadata["body_was_empty"])
        self.assertEqual(
            github.build_github_preview_text("\n\nFirst line\n\nSecond line\n"),
            "First line Second line",
        )

    def test_link_header_pagination_and_bound_reporting(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7, updated_at="2026-03-27T12:00:00Z")], True),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )
        store = github.InMemoryGithubStore()

        result = github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", max_pages_per_stream={"issues": 1}),
            github_client=client,
        )

        issues_progress = result.stream_progress["issues"]
        self.assertEqual(issues_progress.pages_fetched, 1)
        self.assertTrue(issues_progress.hit_bound)
        self.assertEqual(
            issues_progress.latest_source_updated_at,
            datetime(2026, 3, 27, 12, 0, tzinfo=timezone.utc),
        )

    def test_updated_since_is_sent_to_supported_rest_streams_and_filters_unsupported_streams(self):
        embedding_client = FakeEmbeddingClient()
        updated_since = datetime(2026, 3, 28, 0, 0, tzinfo=timezone.utc)
        api_client = github.GithubApiClient(token="token")
        issues_query = parse_qs(
            urlparse(
                api_client._stream_page_url(
                    "issues",
                    bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", updated_since=updated_since),
                    page_number=1,
                )
            ).query
        )
        issue_comments_query = parse_qs(
            urlparse(
                api_client._stream_page_url(
                    "issue_comments",
                    bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", updated_since=updated_since),
                    page_number=1,
                )
            ).query
        )
        pulls_query = parse_qs(
            urlparse(
                api_client._stream_page_url(
                    "pulls",
                    bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", updated_since=updated_since),
                    page_number=1,
                )
            ).query
        )
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7, updated_at="2026-03-28T12:00:00Z")], False),
                ("pulls", 1): ([self._pull_payload(11, updated_at="2026-03-27T12:30:00Z")], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=7)], False),
                ("review_comments", 1): ([self._review_comment_payload(comment_id=2001, pr_number=11, body="review body", updated_at="2026-03-27T14:30:00Z")], False),
            }
        )

        result = github.ingest_github(
            embedding_client=embedding_client,
            github_store=github.InMemoryGithubStore(),
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus", updated_since=updated_since),
            github_client=client,
        )

        sent_bounds = {stream: bounds for stream, bounds in client.page_bounds}
        self.assertEqual(issues_query["since"], ["2026-03-28T00:00:00Z"])
        self.assertEqual(issue_comments_query["since"], ["2026-03-28T00:00:00Z"])
        self.assertNotIn("since", pulls_query)
        self.assertEqual(sent_bounds["issues"].updated_since, updated_since)
        self.assertEqual(sent_bounds["issue_comments"].updated_since, updated_since)
        self.assertEqual(result.issues_written, 1)
        self.assertEqual(result.issue_comments_written, 1)
        self.assertEqual(result.prs_written, 0)
        self.assertEqual(result.pr_comments_written, 0)

    def test_rerun_upserts_refresh_natural_keys_for_all_tables(self):
        store = github.InMemoryGithubStore()
        first_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7, body="first issue body")], False),
                ("pulls", 1): ([self._pull_payload(11, body="first pr body")], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=7, body="first issue comment"), self._issue_comment_payload(comment_id=1002, issue_number=11, body="first pr discussion")], False),
                ("review_comments", 1): ([self._review_comment_payload(comment_id=2001, pr_number=11, body="first review")], False),
            }
        )
        second_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7, body="updated issue body")], False),
                ("pulls", 1): ([self._pull_payload(11, body="updated pr body")], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=7, body="updated issue comment"), self._issue_comment_payload(comment_id=1002, issue_number=11, body="updated pr discussion")], False),
                ("review_comments", 1): ([self._review_comment_payload(comment_id=2001, pr_number=11, body="updated review")], False),
            }
        )
        embedding_client = FakeEmbeddingClient()

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=first_client,
        )
        first_issue_token = store.issues_by_key[("DripDropz/daedalus", 7)]["updated_at_token"]

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=second_client,
        )

        self.assertEqual(len(store.issues_by_key), 1)
        self.assertEqual(len(store.prs_by_key), 1)
        self.assertEqual(len(store.issue_comments_by_key), 1)
        self.assertEqual(len(store.pr_comments_by_key), 2)
        self.assertEqual(store.issues_by_key[("DripDropz/daedalus", 7)]["body_text"], "Issue 7\n\nupdated issue body")
        self.assertEqual(store.prs_by_key[("DripDropz/daedalus", 11)]["body_text"], "PR 11\n\nupdated pr body")
        self.assertEqual(store.issue_comments_by_key[("DripDropz/daedalus", 1001)]["body_text"], "updated issue comment")
        self.assertEqual(store.pr_comments_by_key[("DripDropz/daedalus", "issue_comment", 1002)]["body_text"], "updated pr discussion")
        self.assertEqual(store.pr_comments_by_key[("DripDropz/daedalus", "review_comment", 2001)]["body_text"], "updated review")
        self.assertGreater(
            store.issues_by_key[("DripDropz/daedalus", 7)]["updated_at_token"],
            first_issue_token,
        )

    def test_result_contract_reports_requested_bounds_counts_and_progress(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(7)], False),
                ("pulls", 1): ([self._pull_payload(11)], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=7)], False),
                ("review_comments", 1): ([self._review_comment_payload(comment_id=2001, pr_number=11)], False),
            }
        )
        bounds = github.GithubFetchBounds(
            repo="DripDropz/daedalus",
            updated_since=datetime(2026, 3, 27, 0, 0, tzinfo=timezone.utc),
            page_size=50,
            max_pages=3,
        )

        result = github.ingest_github(
            embedding_client=embedding_client,
            github_store=github.InMemoryGithubStore(),
            bounds=bounds,
            github_client=client,
        )

        self.assertEqual(result.repo, "DripDropz/daedalus")
        self.assertEqual(result.bounds, bounds)
        self.assertEqual(result.issues_written, 1)
        self.assertEqual(result.issue_comments_written, 1)
        self.assertEqual(result.prs_written, 1)
        self.assertEqual(result.pr_comments_written, 1)
        self.assertEqual(set(result.stream_progress), set(github.STREAM_ORDER))
        self.assertEqual(result.stream_progress["issues"].pages_fetched, 1)
        self.assertFalse(result.stream_progress["issues"].hit_bound)

    def test_missing_token_and_config_errors_fail_loudly(self):
        with self.assertRaises(github.MissingGithubTokenError):
            github.GithubApiClient(token="")

        with self.assertRaises(github.MissingGithubTokenError):
            github.ingest_github_from_config(
                config=AgenticConfig(
                    database_url="postgresql://localhost/test",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token=None,
                )
            )

    def test_malformed_response_paths_raise_api_errors(self):
        with self.assertRaises(github.GithubPaginationError):
            github._parse_next_link('<https://api.github.com/resource?page=2>; rel="next", <https://api.github.com/resource?page=3>; rel="next"')

        with self.assertRaises(github.GithubApiResponseError):
            github._parse_timestamp("not-a-timestamp", context="bad")

        class FakeHttpError(HTTPError):
            def __init__(self):
                super().__init__(
                    url="https://api.github.com/repos/example/issues",
                    code=403,
                    msg="Forbidden",
                    hdrs=None,
                    fp=None,
                )

            def read(self):
                return b'{"message":"rate limited"}'

        self.assertEqual(github._read_http_error_detail(FakeHttpError()), "rate limited")
        self.assertEqual(
            github._format_github_timestamp(datetime(2026, 3, 28, 12, 34, 56, 123456, tzinfo=timezone.utc)),
            "2026-03-28T12:34:56Z",
        )

    def test_ingest_github_from_config_uses_store_and_token(self):
        embedding_client = FakeEmbeddingClient()
        captured: dict[str, object] = {}

        class FakeContextStore:
            def __enter__(self):
                return github.InMemoryGithubStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        def fake_from_config(config):
            captured["embed_model"] = config.ollama_embed_model
            return embedding_client

        def fake_store_from_database_url(database_url):
            captured["database_url"] = database_url
            return FakeContextStore()

        def fake_ingest_github(**kwargs):
            captured.update(kwargs)
            return github.GithubIngestResult(
                repo="DripDropz/daedalus",
                bounds=kwargs["bounds"],
                stream_progress={
                    name: github.GithubStreamProgress(
                        stream_name=name,
                        pages_fetched=0,
                        hit_bound=False,
                        latest_source_updated_at=None,
                        issues_written=0,
                        issue_comments_written=0,
                        prs_written=0,
                        pr_comments_written=0,
                    )
                    for name in github.STREAM_ORDER
                },
                issues_written=0,
                issue_comments_written=0,
                prs_written=0,
                pr_comments_written=0,
            )

        config = AgenticConfig(
            database_url="postgresql://localhost/task403",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token="secret-token",
        )
        bounds = github.GithubFetchBounds(repo="DripDropz/daedalus", max_pages=1)

        original_from_config = github.OllamaEmbeddingClient.from_config
        original_store_factory = github.PostgresGithubStore.from_database_url
        original_ingest = github.ingest_github
        try:
            github.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            github.PostgresGithubStore.from_database_url = staticmethod(fake_store_from_database_url)
            github.ingest_github = fake_ingest_github
            result = github.ingest_github_from_config(config=config, bounds=bounds)
        finally:
            github.OllamaEmbeddingClient.from_config = original_from_config
            github.PostgresGithubStore.from_database_url = original_store_factory
            github.ingest_github = original_ingest

        self.assertEqual(result.bounds, bounds)
        self.assertEqual(captured["database_url"], "postgresql://localhost/task403")
        self.assertEqual(captured["github_token"], "secret-token")
        self.assertIs(captured["embedding_client"], embedding_client)

    def _issue_payload(
        self,
        issue_number: int,
        *,
        title: str | None = None,
        body: str = "issue body",
        updated_at: str = "2026-03-28T12:00:00Z",
    ) -> dict:
        return {
            "number": issue_number,
            "node_id": f"ISSUE_{issue_number}",
            "title": title or f"Issue {issue_number}",
            "state": "open",
            "user": {"login": "octocat"},
            "labels": [{"name": "agentic"}],
            "body": body,
            "html_url": f"https://github.com/DripDropz/daedalus/issues/{issue_number}",
            "created_at": "2026-03-27T10:00:00Z",
            "updated_at": updated_at,
            "closed_at": None,
            "author_association": "CONTRIBUTOR",
            "assignees": [{"login": "maintainer"}],
            "milestone": {"title": "Q1"},
            "comments": 2,
            "state_reason": None,
            "locked": False,
        }

    def _issue_stub_for_pr(self, issue_number: int) -> dict:
        payload = self._issue_payload(issue_number, title=f"PR stub {issue_number}")
        payload["pull_request"] = {
            "url": f"https://api.github.com/repos/DripDropz/daedalus/pulls/{issue_number}"
        }
        return payload

    def _pull_payload(
        self,
        pr_number: int,
        *,
        title: str | None = None,
        body: str = "pr body",
        updated_at: str = "2026-03-28T12:30:00Z",
    ) -> dict:
        return {
            "number": pr_number,
            "node_id": f"PR_{pr_number}",
            "title": title or f"PR {pr_number}",
            "state": "open",
            "user": {"login": "octocat"},
            "base": {"ref": "main"},
            "head": {"ref": "feature/task-403"},
            "labels": [{"name": "kb"}],
            "body": body,
            "html_url": f"https://github.com/DripDropz/daedalus/pull/{pr_number}",
            "created_at": "2026-03-27T11:00:00Z",
            "updated_at": updated_at,
            "closed_at": None,
            "merged_at": None,
            "draft": False,
            "merge_commit_sha": None,
            "requested_reviewers": [{"login": "reviewer"}],
            "review_comments": 1,
            "commits": 3,
            "changed_files": 5,
        }

    def _issue_comment_payload(
        self,
        *,
        comment_id: int,
        issue_number: int,
        body: str = "comment body",
        updated_at: str = "2026-03-28T13:30:00Z",
    ) -> dict:
        return {
            "id": comment_id,
            "node_id": f"IC_{comment_id}",
            "user": {"login": "commenter"},
            "body": body,
            "html_url": f"https://github.com/DripDropz/daedalus/issues/{issue_number}#issuecomment-{comment_id}",
            "created_at": "2026-03-28T13:00:00Z",
            "updated_at": updated_at,
            "issue_url": f"https://api.github.com/repos/DripDropz/daedalus/issues/{issue_number}",
        }

    def _review_comment_payload(
        self,
        *,
        comment_id: int,
        pr_number: int,
        body: str = "review body",
        updated_at: str = "2026-03-28T14:30:00Z",
    ) -> dict:
        return {
            "id": comment_id,
            "node_id": f"RC_{comment_id}",
            "user": {"login": "reviewer"},
            "path": "source/main/config.ts",
            "commit_id": "deadbeef",
            "original_commit_id": "cafebabe",
            "diff_hunk": "@@ -40,3 +42,3 @@",
            "line": 42,
            "original_line": 40,
            "side": "RIGHT",
            "start_line": 41,
            "start_side": "RIGHT",
            "body": body,
            "html_url": f"https://github.com/DripDropz/daedalus/pull/{pr_number}#discussion_r{comment_id}",
            "created_at": "2026-03-28T14:00:00Z",
            "updated_at": updated_at,
            "pull_request_url": f"https://api.github.com/repos/DripDropz/daedalus/pulls/{pr_number}",
            "in_reply_to_id": 1999,
            "pull_request_review_id": 9001,
            "position": 7,
            "subject_type": "line",
        }


if __name__ == "__main__":
    unittest.main()
