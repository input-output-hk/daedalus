from __future__ import annotations

import unittest
from datetime import date, datetime, timezone
from urllib.error import HTTPError

from agentic_kb.config import AgenticConfig
from agentic_kb.ingest import project


class FakeEmbeddingClient:
    def __init__(self):
        self.calls: list[list[str]] = []

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        self.calls.append(list(texts))
        return [[float(index)] * 384 for index, _ in enumerate(texts, start=1)]


class FakeGithubProjectApiClient:
    def __init__(self, *, pages: dict[tuple[str | None, int], dict]):
        self.pages = pages
        self.calls: list[tuple[str | None, project.ProjectFetchBounds]] = []
        self.page_number = 0

    def fetch_project_items_page(
        self,
        *,
        bounds: project.ProjectFetchBounds,
        after_cursor: str | None,
    ) -> dict:
        self.page_number += 1
        self.calls.append((after_cursor, bounds))
        try:
            return self.pages[(after_cursor, self.page_number)]
        except KeyError as error:
            raise AssertionError(
                f"Missing fake project page for cursor/page {(after_cursor, self.page_number)!r}"
            ) from error


class ProjectIngestTests(unittest.TestCase):
    def test_iter_project_item_pages_paginates_and_tracks_cursor(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": "https://github.com/orgs/DripDropz/projects/5",
                    "items": [
                        self._project_item_payload(
                            "ITEM_1",
                            content=self._issue_content_payload(7),
                            updated_at="2026-03-28T12:00:00Z",
                        )
                    ],
                    "has_next_page": True,
                    "end_cursor": "cursor-1",
                },
                ("cursor-1", 2): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": "https://github.com/orgs/DripDropz/projects/5",
                    "items": [
                        self._project_item_payload(
                            "ITEM_2",
                            content=self._draft_issue_content_payload(),
                            updated_at="2026-03-28T13:00:00Z",
                        )
                    ],
                    "has_next_page": False,
                    "end_cursor": "cursor-2",
                },
            }
        )

        pages = list(
            project.iter_project_item_pages(
                bounds=project.ProjectFetchBounds(
                    project_owner="DripDropz",
                    project_number=5,
                    page_size=2,
                ),
                embedding_client=embedding_client,
                github_client=client,
            )
        )

        self.assertEqual(len(pages), 2)
        self.assertEqual([page.page_number for page in pages], [1, 2])
        self.assertEqual([page.end_cursor for page in pages], ["cursor-1", "cursor-2"])
        self.assertTrue(pages[0].has_next_page)
        self.assertFalse(pages[1].has_next_page)
        self.assertEqual(client.calls[0][0], None)
        self.assertEqual(client.calls[1][0], "cursor-1")

    def test_canonical_single_select_and_date_fields_map_to_columns(self):
        item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_3",
                content=self._issue_content_payload(8),
                field_values=[
                    self._single_select_field("Status", "In review"),
                    self._single_select_field("Priority", "P1"),
                    self._single_select_field("Size", "M"),
                    self._single_select_field("Work Type", "feature"),
                    self._single_select_field("Area", "ingest"),
                    self._single_select_field("Phase", "Phase 4"),
                    self._single_select_field("KB Impact", "project"),
                    self._date_field("Start date", "2026-03-20"),
                    self._date_field("Target date", "2026-04-02"),
                ],
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url="https://github.com/orgs/DripDropz/projects/5",
            embedding_client=FakeEmbeddingClient(),
        )

        self.assertEqual(item.status, "In review")
        self.assertEqual(item.priority, "P1")
        self.assertEqual(item.size, "M")
        self.assertEqual(item.work_type, "feature")
        self.assertEqual(item.area, "ingest")
        self.assertEqual(item.phase, "Phase 4")
        self.assertEqual(item.kb_impact, "project")
        self.assertEqual(item.start_date, date(2026, 3, 20))
        self.assertEqual(item.target_date, date(2026, 4, 2))
        self.assertEqual(item.field_values["Status"]["value"], "In review")
        self.assertEqual(item.field_values["Target date"]["value"], "2026-04-02")

    def test_content_types_map_issue_pr_draft_and_non_daedalus_linkage(self):
        embedding_client = FakeEmbeddingClient()
        issue_item = project._prepare_project_item(
            self._project_item_payload("ITEM_ISSUE", content=self._issue_content_payload(7)),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )
        pr_item = project._prepare_project_item(
            self._project_item_payload("ITEM_PR", content=self._pull_request_content_payload(11)),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )
        draft_item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_DRAFT",
                content=self._draft_issue_content_payload(title="Draft planning", body="draft body"),
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )
        foreign_repo_item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_FOREIGN",
                content=self._issue_content_payload(99, repo="DripDropz/other-repo"),
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )

        self.assertEqual(issue_item.content_type, "issue")
        self.assertEqual(issue_item.content_id, "github-issue:DripDropz/daedalus#7")
        self.assertEqual(pr_item.content_type, "pull_request")
        self.assertEqual(pr_item.content_id, "github-pr:DripDropz/daedalus#11")
        self.assertEqual(draft_item.content_type, "draft_issue")
        self.assertIsNone(draft_item.content_id)
        self.assertEqual(foreign_repo_item.repo, "DripDropz/other-repo")
        self.assertIsNone(foreign_repo_item.content_id)

    def test_body_text_includes_field_summary_and_preview_is_derivable(self):
        item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_4",
                content=self._issue_content_payload(7, body="Issue body"),
                field_values=[
                    self._single_select_field("Status", "Ready"),
                    self._single_select_field("KB Impact", "project"),
                ],
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=FakeEmbeddingClient(),
        )

        self.assertIn("Issue 7", item.body_text)
        self.assertIn("Issue body", item.body_text)
        self.assertIn("Project fields:", item.body_text)
        self.assertIn("Status: Ready", item.body_text)
        self.assertIn("KB Impact: project", item.body_text)
        self.assertEqual(
            project.build_project_preview_text(item.body_text),
            "Issue 7 Issue body Project fields: Status: Ready KB Impact: project",
        )

    def test_rerun_upserts_refresh_rows_by_project_item_node_id(self):
        store = project.InMemoryProjectItemsStore()
        embedding_client = FakeEmbeddingClient()
        first_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_5",
                            content=self._issue_content_payload(7, body="first body"),
                        )
                    ],
                    "has_next_page": False,
                    "end_cursor": None,
                }
            }
        )
        second_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_5",
                            content=self._issue_content_payload(7, body="updated body"),
                        )
                    ],
                    "has_next_page": False,
                    "end_cursor": None,
                }
            }
        )

        project.ingest_project_items(
            embedding_client=embedding_client,
            project_store=store,
            bounds=project.ProjectFetchBounds(project_owner="DripDropz", project_number=5),
            github_client=first_client,
        )
        first_token = store.rows_by_key["ITEM_5"]["updated_at_token"]

        project.ingest_project_items(
            embedding_client=embedding_client,
            project_store=store,
            bounds=project.ProjectFetchBounds(project_owner="DripDropz", project_number=5),
            github_client=second_client,
        )

        self.assertEqual(len(store.rows_by_key), 1)
        self.assertEqual(
            store.rows_by_key["ITEM_5"]["body_text"],
            "Issue 7\n\nupdated body\n\nProject fields:\nStatus: Backlog\nPriority: P2",
        )
        self.assertGreater(store.rows_by_key["ITEM_5"]["updated_at_token"], first_token)

    def test_result_contract_reports_bounds_pages_cursors_and_updates(self):
        embedding_client = FakeEmbeddingClient()
        client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": "https://github.com/orgs/DripDropz/projects/5",
                    "items": [
                        self._project_item_payload(
                            "ITEM_6",
                            content=self._issue_content_payload(7),
                            updated_at="2026-03-28T15:00:00Z",
                        )
                    ],
                    "has_next_page": True,
                    "end_cursor": "cursor-6",
                }
            }
        )
        bounds = project.ProjectFetchBounds(
            project_owner="DripDropz",
            project_number=5,
            page_size=10,
            max_pages=1,
        )

        result = project.ingest_project_items(
            embedding_client=embedding_client,
            project_store=project.InMemoryProjectItemsStore(),
            bounds=bounds,
            github_client=client,
        )

        self.assertEqual(result.project_owner, "DripDropz")
        self.assertEqual(result.project_number, 5)
        self.assertEqual(result.project_title, "Daedalus Maintenance")
        self.assertEqual(result.project_url, "https://github.com/orgs/DripDropz/projects/5")
        self.assertEqual(result.bounds, bounds)
        self.assertEqual(result.pages_fetched, 1)
        self.assertTrue(result.hit_bound)
        self.assertEqual(result.final_cursor, "cursor-6")
        self.assertEqual(result.rows_written, 1)
        self.assertEqual(
            result.latest_source_updated_at,
            datetime(2026, 3, 28, 15, 0, tzinfo=timezone.utc),
        )

    def test_missing_token_and_config_errors_fail_loudly(self):
        with self.assertRaises(project.MissingProjectGithubTokenError):
            project.GithubProjectApiClient(token="")

        with self.assertRaises(project.MissingProjectGithubTokenError):
            project.ingest_project_items_from_config(
                config=AgenticConfig(
                    database_url="postgresql://localhost/task404",
                    ollama_base_url="http://ollama:11434",
                    ollama_embed_model="all-minilm",
                    github_token=None,
                )
            )

    def test_graphql_errors_malformed_payloads_and_nested_field_pagination_fail(self):
        with self.assertRaises(project.ProjectGithubGraphQLError):
            project._require_graphql_data(
                {"errors": [{"message": "Resource not accessible by integration"}]}
            )

        with self.assertRaises(project.ProjectGithubApiResponseError):
            project._parse_timestamp("bad", context="project item")

        with self.assertRaises(project.ProjectFieldPaginationError):
            project._prepare_project_item(
                self._project_item_payload(
                    "ITEM_7",
                    content=self._issue_content_payload(7),
                    field_has_next_page=True,
                ),
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url=None,
                embedding_client=FakeEmbeddingClient(),
            )

        with self.assertRaises(project.ProjectGithubApiResponseError):
            project._prepare_project_item(
                self._project_item_payload(
                    "ITEM_8",
                    content=self._issue_content_payload(7),
                    field_values=[self._date_field("Status", "2026-03-28")],
                ),
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url=None,
                embedding_client=FakeEmbeddingClient(),
            )

    def test_ingest_project_items_from_config_uses_store_and_token(self):
        embedding_client = FakeEmbeddingClient()
        captured: dict[str, object] = {}

        class FakeContextStore:
            def __enter__(self):
                return project.InMemoryProjectItemsStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        class FakeSyncContextStore:
            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def record_attempts(self, attempts):
                return len(attempts)

            def upsert_sync_states(self, states):
                return len(states)

            def record_failures(self, failures):
                return len(failures)

        def fake_from_config(config):
            captured["embed_model"] = config.ollama_embed_model
            return embedding_client

        def fake_store_from_database_url(database_url):
            captured["database_url"] = database_url
            return FakeContextStore()

        def fake_sync_store_from_database_url(database_url):
            captured["sync_database_url"] = database_url
            return FakeSyncContextStore()

        def fake_ingest_project_items(**kwargs):
            captured.update(kwargs)
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=0,
                hit_bound=False,
                final_cursor=None,
                latest_source_updated_at=None,
                rows_written=0,
            )

        config = AgenticConfig(
            database_url="postgresql://localhost/task404",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token="secret-token",
        )
        bounds = project.ProjectFetchBounds(project_owner="DripDropz", project_number=5)

        original_from_config = project.OllamaEmbeddingClient.from_config
        original_store_factory = project.PostgresProjectItemsStore.from_database_url
        original_ingest = project.ingest_project_items
        original_sync_store_factory = project.PostgresSyncStateStore.from_database_url
        try:
            project.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            project.PostgresProjectItemsStore.from_database_url = staticmethod(
                fake_store_from_database_url
            )
            project.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            project.ingest_project_items = fake_ingest_project_items
            result = project.ingest_project_items_from_config(config=config, bounds=bounds)
        finally:
            project.OllamaEmbeddingClient.from_config = original_from_config
            project.PostgresProjectItemsStore.from_database_url = original_store_factory
            project.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            project.ingest_project_items = original_ingest

        self.assertEqual(result.bounds, bounds)
        self.assertEqual(captured["database_url"], "postgresql://localhost/task404")
        self.assertEqual(captured["sync_database_url"], "postgresql://localhost/task404")
        self.assertEqual(captured["github_token"], "secret-token")
        self.assertIs(captured["embedding_client"], embedding_client)

    def test_ingest_project_items_from_config_records_sync_state_attempt_and_success(self):
        embedding_client = FakeEmbeddingClient()
        captured: dict[str, object] = {}

        class FakeProjectContextStore:
            def __enter__(self):
                return project.InMemoryProjectItemsStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        class FakeSyncContextStore:
            def __init__(self):
                self.attempts = []
                self.states = []
                self.failures = []

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def record_attempts(self, attempts):
                self.attempts.extend(attempts)
                return len(attempts)

            def upsert_sync_states(self, states):
                self.states.extend(states)
                return len(states)

            def record_failures(self, failures):
                self.failures.extend(failures)
                return len(failures)

        sync_store = FakeSyncContextStore()

        def fake_from_config(config):
            return embedding_client

        def fake_store_from_database_url(database_url):
            captured["database_url"] = database_url
            return FakeProjectContextStore()

        def fake_sync_store_from_database_url(database_url):
            captured["sync_database_url"] = database_url
            return sync_store

        def fake_ingest_project_items(**kwargs):
            return project.ProjectIngestResult(
                project_owner="DripDropz",
                project_number=5,
                project_title="Daedalus Maintenance",
                project_url="https://github.com/orgs/DripDropz/projects/5",
                bounds=kwargs["bounds"],
                pages_fetched=1,
                hit_bound=False,
                final_cursor="cursor-123",
                latest_source_updated_at=datetime(2026, 3, 28, 16, 0, tzinfo=timezone.utc),
                rows_written=1,
            )

        config = AgenticConfig(
            database_url="postgresql://localhost/task404",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token="secret-token",
        )
        bounds = project.ProjectFetchBounds(project_owner="DripDropz", project_number=5)

        original_from_config = project.OllamaEmbeddingClient.from_config
        original_store_factory = project.PostgresProjectItemsStore.from_database_url
        original_sync_store_factory = project.PostgresSyncStateStore.from_database_url
        original_ingest = project.ingest_project_items
        try:
            project.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            project.PostgresProjectItemsStore.from_database_url = staticmethod(
                fake_store_from_database_url
            )
            project.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            project.ingest_project_items = fake_ingest_project_items
            result = project.ingest_project_items_from_config(config=config, bounds=bounds)
        finally:
            project.OllamaEmbeddingClient.from_config = original_from_config
            project.PostgresProjectItemsStore.from_database_url = original_store_factory
            project.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            project.ingest_project_items = original_ingest

        self.assertEqual(result.final_cursor, "cursor-123")
        self.assertEqual(captured["sync_database_url"], "postgresql://localhost/task404")
        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.states), 1)
        self.assertEqual(sync_store.failures, [])
        self.assertEqual(sync_store.states[0].cursor_text, "cursor-123")
        self.assertEqual(sync_store.states[0].watermark_text, "2026-03-28T16:00:00Z")

    def test_ingest_project_items_from_config_records_sync_failure_before_reraising(self):
        class FakeProjectContextStore:
            def __enter__(self):
                return project.InMemoryProjectItemsStore()

            def __exit__(self, exc_type, exc, tb):
                return False

        class FakeSyncContextStore:
            def __init__(self):
                self.attempts = []
                self.failures = []

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def record_attempts(self, attempts):
                self.attempts.extend(attempts)
                return len(attempts)

            def upsert_sync_states(self, states):
                raise AssertionError("should not persist success state on failure")

            def record_failures(self, failures):
                self.failures.extend(failures)
                return len(failures)

        sync_store = FakeSyncContextStore()

        def fake_from_config(config):
            return FakeEmbeddingClient()

        def fake_store_from_database_url(database_url):
            return FakeProjectContextStore()

        def fake_sync_store_from_database_url(database_url):
            return sync_store

        def fake_ingest_project_items(**kwargs):
            raise RuntimeError("boom from project ingest")

        config = AgenticConfig(
            database_url="postgresql://localhost/task404",
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token="secret-token",
        )

        original_from_config = project.OllamaEmbeddingClient.from_config
        original_store_factory = project.PostgresProjectItemsStore.from_database_url
        original_sync_store_factory = project.PostgresSyncStateStore.from_database_url
        original_ingest = project.ingest_project_items
        try:
            project.OllamaEmbeddingClient.from_config = staticmethod(fake_from_config)
            project.PostgresProjectItemsStore.from_database_url = staticmethod(
                fake_store_from_database_url
            )
            project.PostgresSyncStateStore.from_database_url = staticmethod(
                fake_sync_store_from_database_url
            )
            project.ingest_project_items = fake_ingest_project_items
            with self.assertRaisesRegex(RuntimeError, "boom from project ingest"):
                project.ingest_project_items_from_config(config=config)
        finally:
            project.OllamaEmbeddingClient.from_config = original_from_config
            project.PostgresProjectItemsStore.from_database_url = original_store_factory
            project.PostgresSyncStateStore.from_database_url = original_sync_store_factory
            project.ingest_project_items = original_ingest

        self.assertEqual(len(sync_store.attempts), 1)
        self.assertEqual(len(sync_store.failures), 1)
        self.assertIn("boom from project ingest", sync_store.failures[0].error)

    def test_helpers_cover_ids_preview_and_http_error_detail(self):
        self.assertEqual(
            project.deterministic_project_item_id("DripDropz", 5, "ITEM_9"),
            "github-project-item:DripDropz/5:ITEM_9",
        )
        self.assertEqual(
            project.build_project_preview_text("\n\nFirst line\n\nSecond line\n"),
            "First line Second line",
        )

        class FakeHttpError(HTTPError):
            def __init__(self):
                super().__init__(
                    url="https://api.github.com/graphql",
                    code=403,
                    msg="Forbidden",
                    hdrs=None,
                    fp=None,
                )

            def read(self):
                return b'{"message":"project scope missing"}'

        self.assertEqual(
            project._read_http_error_detail(FakeHttpError()),
            "project scope missing",
        )

    def _project_item_payload(
        self,
        node_id: str,
        *,
        content: dict | None,
        field_values: list[dict] | None = None,
        field_has_next_page: bool = False,
        updated_at: str = "2026-03-28T12:00:00Z",
    ) -> dict:
        return {
            "id": node_id,
            "updatedAt": updated_at,
            "isArchived": False,
            "fieldValues": {
                "pageInfo": {
                    "hasNextPage": field_has_next_page,
                    "endCursor": "field-cursor" if field_has_next_page else None,
                },
                "nodes": field_values
                or [
                    self._single_select_field("Status", "Backlog"),
                    self._single_select_field("Priority", "P2"),
                ],
            },
            "content": content,
        }

    def _issue_content_payload(
        self,
        number: int,
        *,
        repo: str = "DripDropz/daedalus",
        title: str | None = None,
        body: str = "issue body",
    ) -> dict:
        return {
            "__typename": "Issue",
            "id": f"ISSUE_{number}",
            "title": title or f"Issue {number}",
            "body": body,
            "url": f"https://github.com/{repo}/issues/{number}",
            "number": number,
            "state": "OPEN",
            "createdAt": "2026-03-27T10:00:00Z",
            "updatedAt": "2026-03-28T11:00:00Z",
            "closedAt": None,
            "repository": {"nameWithOwner": repo},
            "author": {"login": "octocat"},
        }

    def _pull_request_content_payload(
        self,
        number: int,
        *,
        repo: str = "DripDropz/daedalus",
        title: str | None = None,
        body: str = "pr body",
    ) -> dict:
        return {
            "__typename": "PullRequest",
            "id": f"PR_{number}",
            "title": title or f"PR {number}",
            "body": body,
            "url": f"https://github.com/{repo}/pull/{number}",
            "number": number,
            "state": "OPEN",
            "createdAt": "2026-03-27T10:00:00Z",
            "updatedAt": "2026-03-28T11:00:00Z",
            "closedAt": None,
            "mergedAt": None,
            "repository": {"nameWithOwner": repo},
            "author": {"login": "octocat"},
        }

    def _draft_issue_content_payload(
        self,
        *,
        title: str = "Draft issue",
        body: str = "draft issue body",
    ) -> dict:
        return {
            "__typename": "DraftIssue",
            "id": "DRAFT_1",
            "title": title,
            "body": body,
        }

    def _single_select_field(self, field_name: str, option_name: str) -> dict:
        return {
            "__typename": "ProjectV2ItemFieldSingleSelectValue",
            "name": option_name,
            "optionId": f"option-{field_name}",
            "field": {
                "id": f"FIELD_{field_name}",
                "name": field_name,
                "dataType": "SINGLE_SELECT",
            },
        }

    def _date_field(self, field_name: str, raw_date: str) -> dict:
        return {
            "__typename": "ProjectV2ItemFieldDateValue",
            "date": raw_date,
            "field": {
                "id": f"FIELD_{field_name}",
                "name": field_name,
                "dataType": "DATE",
            },
        }


if __name__ == "__main__":
    unittest.main()
