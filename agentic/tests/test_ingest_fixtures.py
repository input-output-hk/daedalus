from __future__ import annotations

import tempfile
import time
import unittest
from datetime import date, datetime, timezone
from pathlib import Path

from agentic_kb.ingest import docs, code, github, project

from agentic.tests.test_docs_ingest import FakeEmbeddingClient as DocsFakeEmbeddingClient
from agentic_kb.ingest.docs import InMemoryDocsStore
from agentic_kb.ingest.code import InMemoryCodeChunksStore
from agentic.tests.test_github_ingest import FakeGithubApiClient
from agentic_kb.ingest.github import InMemoryGithubStore
from agentic.tests.test_project_ingest import FakeGithubProjectApiClient
from agentic_kb.ingest.project import InMemoryProjectItemsStore


REPO_ROOT = Path(__file__).resolve().parents[2]


class DocsFixturesTests(unittest.TestCase):
    def test_canonical_task_plan_metadata_extraction(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712.md",
                "# Task Plan: task-712 Add ingest fixture coverage\n"
                "- Task ID: `task-712`\n"
                "- Title: `Add ingest fixture coverage`\n"
                "- Planning Status: `approved`\n"
                "- Build Status: `in_progress`\n\n"
                "## Scope\n"
                "- Create fixture tests.\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            prepared = docs.prepare_documents(
                workspace,
                source_paths=[".agent/plans/agentic/task-plans/task-712.md"],
                embedding_client=embedding_client,
                repo_commit_hash="fixture-commit",
            )

        document = prepared[0]
        self.assertEqual(document.metadata["task_id"], "task-712")
        self.assertEqual(document.metadata["title"], "Add ingest fixture coverage")
        self.assertEqual(document.metadata["planning_status"], "approved")
        self.assertEqual(document.metadata["build_status"], "in_progress")
        self.assertEqual(document.metadata["plan_type"], "canonical_task_plan")

    def test_plan_type_discrimination(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712.md",
                "# Task Plan: task-712\n- Task ID: `task-712`\n",
            )
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712-plan-review.md",
                "Planner: Iteration 1\nTimestamp: 2026-03-30T22:00:00Z\n",
            )
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712-impl-review.md",
                "Implementation: Iteration 1\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            prepared = docs.prepare_documents(
                workspace,
                source_paths=[
                    ".agent/plans/agentic/task-plans/task-712.md",
                    ".agent/plans/agentic/task-plans/task-712-plan-review.md",
                    ".agent/plans/agentic/task-plans/task-712-impl-review.md",
                ],
                embedding_client=embedding_client,
                repo_commit_hash="discrimination-commit",
            )

        rows = {doc.source_path: doc for doc in prepared}
        self.assertEqual(rows[".agent/plans/agentic/task-plans/task-712.md"].metadata["plan_type"], "canonical_task_plan")
        self.assertEqual(rows[".agent/plans/agentic/task-plans/task-712-plan-review.md"].metadata["plan_type"], "plan_review_log")
        self.assertEqual(rows[".agent/plans/agentic/task-plans/task-712-impl-review.md"].metadata["plan_type"], "implementation_review_log")
        self.assertNotIn("task_id", rows[".agent/plans/agentic/task-plans/task-712-plan-review.md"].metadata)
        self.assertNotIn("planning_status", rows[".agent/plans/agentic/task-plans/task-712-impl-review.md"].metadata)

    def test_idempotent_reingest_unchanged_plan_doc_content_hash_equality(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712.md",
                "# Task Plan: task-712\n- Task ID: `task-712`\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryDocsStore()

            first_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )

            first_row = dict(store.rows_by_key[(".agent/plans/agentic/task-plans/task-712.md", 0)])
            first_content_hash = first_row["content_hash"]

            time.sleep(0.02)

            second_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

            second_row = dict(store.rows_by_key[(".agent/plans/agentic/task-plans/task-712.md", 0)])
            second_content_hash = second_row["content_hash"]

        self.assertEqual(second_result.updated_paths, ())
        self.assertEqual(second_result.skipped_paths, (".agent/plans/agentic/task-plans/task-712.md",))
        self.assertEqual(first_content_hash, second_content_hash)

    def test_idempotent_reingest_changed_plan_doc_in_place_replacement(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            plan_path = workspace / ".agent/plans/agentic/task-plans/task-712.md"
            self._write_file(
                plan_path,
                "# Task Plan: task-712\n- Task ID: `task-712`\n- Title: `Original`\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryDocsStore()

            first_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )

            first_row = dict(store.rows_by_key[(".agent/plans/agentic/task-plans/task-712.md", 0)])
            first_content_hash = first_row["content_hash"]

            self._write_file(
                plan_path,
                "# Task Plan: task-712\n- Task ID: `task-712`\n- Title: `Updated`\n",
            )

            second_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

            second_row = dict(store.rows_by_key[(".agent/plans/agentic/task-plans/task-712.md", 0)])

        self.assertEqual(second_result.updated_paths, (".agent/plans/agentic/task-plans/task-712.md",))
        self.assertNotEqual(first_content_hash, second_row["content_hash"])
        self.assertEqual(second_row["metadata"]["title"], "Updated")

    def test_store_key_uniqueness_under_docs_reingest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-712.md",
                "# Task Plan: task-712\n- Task ID: `task-712`\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryDocsStore()

            docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )

            docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

            all_keys = list(store.rows_by_key.keys())

        source_path_keys = [key for key in all_keys if key[0] == ".agent/plans/agentic/task-plans/task-712.md"]
        self.assertEqual(len(source_path_keys), len(set(source_path_keys)))

    def test_heading_boundary_chunking_consistency_across_reingest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "README.md",
                "# Title\n\n"
                "Intro paragraph.\n\n"
                "## First Section\n"
                "Alpha body.\n\n"
                "## Second Section\n"
                "Omega body.\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            first_prepared = docs.prepare_documents(
                workspace,
                source_paths=["README.md"],
                embedding_client=embedding_client,
                repo_commit_hash="chunk-commit-a",
            )

            embedding_client2 = DocsFakeEmbeddingClient()
            second_prepared = docs.prepare_documents(
                workspace,
                source_paths=["README.md"],
                embedding_client=embedding_client2,
                repo_commit_hash="chunk-commit-b",
            )

        self.assertEqual(len(first_prepared), len(second_prepared))
        self.assertEqual(
            [doc.heading_path for doc in first_prepared],
            [doc.heading_path for doc in second_prepared],
        )
        self.assertEqual(
            [doc.chunk_index for doc in first_prepared],
            [doc.chunk_index for doc in second_prepared],
        )

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


class CodeFixturesTests(unittest.TestCase):
    def test_language_classification_accuracy(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "source/main/api.ts", "export const API = 'test';\n")
            self._write_file(workspace / "source/renderer/View.tsx", "export const View = () => null;\n")
            self._write_file(workspace / "translations/config.json", '{"key":"value"}\n')
            self._write_file(workspace / ".buildkite/pipeline.yml", "steps:\n  - label: test\n")
            self._write_file(workspace / "Dockerfile", "FROM python:3.12\n")

            embedding_client = DocsFakeEmbeddingClient()
            prepared = code.prepare_code_chunks(
                workspace,
                source_paths=[
                    "source/main/api.ts",
                    "source/renderer/View.tsx",
                    "translations/config.json",
                    ".buildkite/pipeline.yml",
                    "Dockerfile",
                ],
                embedding_client=embedding_client,
                repo_commit_hash="lang-commit",
            )

        chunks_by_path = {chunk.repo_path: chunk for chunk in prepared}
        self.assertEqual(chunks_by_path["source/main/api.ts"].language, "typescript")
        self.assertEqual(chunks_by_path["source/renderer/View.tsx"].language, "typescriptreact")
        self.assertEqual(chunks_by_path["translations/config.json"].language, "json")
        self.assertIn(chunks_by_path[".buildkite/pipeline.yml"].language, ("yaml", "config"))
        self.assertEqual(chunks_by_path["Dockerfile"].language, "config")

    def test_symbol_chunk_consistency_across_reingest(self):
        ts_content = (
            "export const Alpha = 1;\n"
            "export function Beta() {\n"
            "  return Alpha;\n"
            "}\n"
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            target = workspace / "source/main/fixture.ts"
            self._write_file(target, ts_content)

            embedding_client = DocsFakeEmbeddingClient()
            first_prepared = code.prepare_code_chunks(
                workspace,
                source_paths=["source/main/fixture.ts"],
                embedding_client=embedding_client,
                repo_commit_hash="symbol-commit-a",
            )

            embedding_client2 = DocsFakeEmbeddingClient()
            second_prepared = code.prepare_code_chunks(
                workspace,
                source_paths=["source/main/fixture.ts"],
                embedding_client=embedding_client2,
                repo_commit_hash="symbol-commit-b",
            )

        first_symbols = {chunk.symbol_name for chunk in first_prepared if chunk.symbol_name}
        second_symbols = {chunk.symbol_name for chunk in second_prepared if chunk.symbol_name}
        self.assertEqual(first_symbols, second_symbols)
        self.assertEqual(
            [chunk.chunk_index for chunk in first_prepared],
            [chunk.chunk_index for chunk in second_prepared],
        )

    def test_idempotent_reingest_unchanged_code_content_hash_equality(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "source/main/fixture.ts",
                "export const Alpha = 1;\nexport function Beta() { return Alpha; }\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryCodeChunksStore()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-a",
            )

            first_chunks = {
                key: dict(row) for key, row in store.rows_by_key.items() if key[0] == "source/main/fixture.ts"
            }
            first_content_hashes = {key: row["content_hash"] for key, row in first_chunks.items()}

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-b",
            )

            second_chunks = {
                key: dict(row) for key, row in store.rows_by_key.items() if key[0] == "source/main/fixture.ts"
            }
            second_content_hashes = {key: row["content_hash"] for key, row in second_chunks.items()}

        self.assertEqual(first_content_hashes, second_content_hashes)

    def test_idempotent_reingest_changed_code_in_place_replacement(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            target = workspace / "source/main/fixture.ts"
            self._write_file(target, "export const Alpha = 1;\n")

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryCodeChunksStore()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-a",
            )

            first_chunks = {
                key: dict(row) for key, row in store.rows_by_key.items() if key[0] == "source/main/fixture.ts"
            }
            first_content_hash = first_chunks[("source/main/fixture.ts", 0)]["content_hash"]

            self._write_file(target, "export const Gamma = 2;\n")

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-b",
            )

            second_chunks = {
                key: dict(row) for key, row in store.rows_by_key.items() if key[0] == "source/main/fixture.ts"
            }

        self.assertNotEqual(first_content_hash, second_chunks[("source/main/fixture.ts", 0)]["content_hash"])
        self.assertEqual(second_chunks[("source/main/fixture.ts", 0)]["symbol_name"], "Gamma")

    def test_store_key_uniqueness_under_code_reingest(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "source/main/fixture.ts",
                "export const Alpha = 1;\n",
            )

            embedding_client = DocsFakeEmbeddingClient()
            store = InMemoryCodeChunksStore()

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-a",
            )

            code.ingest_code(
                workspace,
                embedding_client=embedding_client,
                code_store=store,
                source_paths=["source/main/fixture.ts"],
                repo_commit_hash="commit-b",
            )

            all_keys = list(store.rows_by_key.keys())

        path_keys = [key for key in all_keys if key[0] == "source/main/fixture.ts"]
        self.assertEqual(len(path_keys), len(set(path_keys)))

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


class GithubFixturesTests(unittest.TestCase):
    def test_issue_metadata_extraction(self):
        embedding_client = DocsFakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712, title="Fixture issue", body="issue body")], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )
        store = InMemoryGithubStore()

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=client,
        )

        issue_row = store.issues_by_key[("DripDropz/daedalus", 712)]
        self.assertEqual(issue_row["issue_number"], 712)
        self.assertEqual(issue_row["title"], "Fixture issue")
        self.assertIn("agentic", issue_row["labels"])
        self.assertIn("issue body", issue_row["body_text"])

    def test_pr_metadata_extraction(self):
        embedding_client = DocsFakeEmbeddingClient()
        client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([], False),
                ("pulls", 1): ([self._pull_payload(401, title="Fixture PR", body="pr body")], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )
        store = InMemoryGithubStore()

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=client,
        )

        pr_row = store.prs_by_key[("DripDropz/daedalus", 401)]
        self.assertEqual(pr_row["pr_number"], 401)
        self.assertEqual(pr_row["title"], "Fixture PR")
        self.assertEqual(pr_row["base_branch"], "main")
        self.assertEqual(pr_row["head_branch"], "feature/task-403")

    def test_idempotent_reingest_unchanged_issue_token_increments(self):
        store = InMemoryGithubStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712, body="stable body")], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=first_client,
        )

        first_row = dict(store.issues_by_key[("DripDropz/daedalus", 712)])
        first_token = first_row["updated_at_token"]

        second_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712, body="stable body")], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=second_client,
        )

        second_row = dict(store.issues_by_key[("DripDropz/daedalus", 712)])

        self.assertGreater(second_row["updated_at_token"], first_token)
        self.assertEqual(first_row["body_text"], second_row["body_text"])

    def test_idempotent_reingest_changed_issue_updates_in_place(self):
        store = InMemoryGithubStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712, body="original body")], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=first_client,
        )

        first_row = dict(store.issues_by_key[("DripDropz/daedalus", 712)])
        first_token = first_row["updated_at_token"]

        second_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712, body="updated body")], False),
                ("pulls", 1): ([], False),
                ("issue_comments", 1): ([], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=second_client,
        )

        second_row = dict(store.issues_by_key[("DripDropz/daedalus", 712)])

        self.assertGreater(second_row["updated_at_token"], first_token)
        self.assertIn("updated body", second_row["body_text"])

    def test_store_key_uniqueness_under_github_reingest(self):
        store = InMemoryGithubStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712)], False),
                ("pulls", 1): ([self._pull_payload(401)], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=712)], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=first_client,
        )

        second_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712)], False),
                ("pulls", 1): ([self._pull_payload(401)], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=712)], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=second_client,
        )

        self.assertEqual(len(store.issues_by_key), 1)
        self.assertEqual(len(store.prs_by_key), 1)
        self.assertEqual(len(store.issue_comments_by_key), 1)

    def test_comment_routing_consistency_across_reingest(self):
        store = InMemoryGithubStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubApiClient(
            pages={
                ("issues", 1): ([self._issue_payload(712)], False),
                ("pulls", 1): ([self._pull_payload(401)], False),
                ("issue_comments", 1): ([self._issue_comment_payload(comment_id=1001, issue_number=712), self._issue_comment_payload(comment_id=1002, issue_number=401)], False),
                ("review_comments", 1): ([], False),
            }
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=first_client,
        )

        github.ingest_github(
            embedding_client=embedding_client,
            github_store=store,
            bounds=github.GithubFetchBounds(repo="DripDropz/daedalus"),
            github_client=FakeGithubApiClient(
                pages={
                    ("issues", 1): ([], False),
                    ("pulls", 1): ([], False),
                    ("issue_comments", 1): ([], False),
                    ("review_comments", 1): ([], False),
                }
            ),
        )

        self.assertEqual(len(store.issue_comments_by_key), 1)
        self.assertEqual(len(store.pr_comments_by_key), 1)
        self.assertIn(("DripDropz/daedalus", 1001), store.issue_comments_by_key)
        self.assertIn(("DripDropz/daedalus", "issue_comment", 1002), store.pr_comments_by_key)

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
            "issue_url": f"https://github.com/DripDropz/daedalus/issues/{issue_number}",
        }


class ProjectFixturesTests(unittest.TestCase):
    def test_single_select_field_extraction(self):
        embedding_client = DocsFakeEmbeddingClient()
        item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_FIXTURE",
                content=self._issue_content_payload(712),
                field_values=[
                    self._single_select_field("Status", "In review"),
                    self._single_select_field("Priority", "P1"),
                    self._single_select_field("Size", "M"),
                    self._single_select_field("Work Type", "feature"),
                    self._single_select_field("Area", "ingest"),
                    self._single_select_field("Phase", "Phase 4"),
                    self._single_select_field("KB Impact", "project"),
                ],
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url="https://github.com/orgs/DripDropz/projects/5",
            embedding_client=embedding_client,
        )

        self.assertEqual(item.status, "In review")
        self.assertEqual(item.priority, "P1")
        self.assertEqual(item.size, "M")
        self.assertEqual(item.work_type, "feature")
        self.assertEqual(item.area, "ingest")
        self.assertEqual(item.phase, "Phase 4")
        self.assertEqual(item.kb_impact, "project")

    def test_date_field_extraction(self):
        embedding_client = DocsFakeEmbeddingClient()
        item = project._prepare_project_item(
            self._project_item_payload(
                "ITEM_FIXTURE",
                content=self._issue_content_payload(712),
                field_values=[
                    self._date_field("Start date", "2026-03-20"),
                    self._date_field("Target date", "2026-04-02"),
                ],
            ),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url="https://github.com/orgs/DripDropz/projects/5",
            embedding_client=embedding_client,
        )

        self.assertEqual(item.start_date, date(2026, 3, 20))
        self.assertEqual(item.target_date, date(2026, 4, 2))

    def test_content_type_discrimination(self):
        embedding_client = DocsFakeEmbeddingClient()
        issue_item = project._prepare_project_item(
            self._project_item_payload("ITEM_ISSUE", content=self._issue_content_payload(712)),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )
        pr_item = project._prepare_project_item(
            self._project_item_payload("ITEM_PR", content=self._pull_request_content_payload(401)),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )
        draft_item = project._prepare_project_item(
            self._project_item_payload("ITEM_DRAFT", content=self._draft_issue_content_payload()),
            project_owner="DripDropz",
            project_number=5,
            project_title="Daedalus Maintenance",
            project_url=None,
            embedding_client=embedding_client,
        )

        self.assertEqual(issue_item.content_type, "issue")
        self.assertEqual(issue_item.content_id, "github-issue:DripDropz/daedalus#712")
        self.assertEqual(pr_item.content_type, "pull_request")
        self.assertEqual(pr_item.content_id, "github-pr:DripDropz/daedalus#401")
        self.assertEqual(draft_item.content_type, "draft_issue")
        self.assertIsNone(draft_item.content_id)

    def test_idempotent_reingest_unchanged_project_item_token_increments(self):
        store = InMemoryProjectItemsStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_FIXTURE",
                            content=self._issue_content_payload(712, body="stable body"),
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

        first_row = dict(store.rows_by_key["ITEM_FIXTURE"])
        first_token = first_row["updated_at_token"]

        second_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_FIXTURE",
                            content=self._issue_content_payload(712, body="stable body"),
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
            github_client=second_client,
        )

        second_row = dict(store.rows_by_key["ITEM_FIXTURE"])

        self.assertGreater(second_row["updated_at_token"], first_token)
        self.assertEqual(first_row["body_text"], second_row["body_text"])

    def test_idempotent_reingest_changed_project_item_updates_in_place(self):
        store = InMemoryProjectItemsStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_FIXTURE",
                            content=self._issue_content_payload(712, body="original body"),
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

        first_row = dict(store.rows_by_key["ITEM_FIXTURE"])
        first_token = first_row["updated_at_token"]

        second_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload(
                            "ITEM_FIXTURE",
                            content=self._issue_content_payload(712, body="updated body"),
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
            github_client=second_client,
        )

        second_row = dict(store.rows_by_key["ITEM_FIXTURE"])

        self.assertGreater(second_row["updated_at_token"], first_token)
        self.assertIn("updated body", second_row["body_text"])

    def test_store_key_uniqueness_under_project_reingest(self):
        store = InMemoryProjectItemsStore()
        embedding_client = DocsFakeEmbeddingClient()

        first_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload("ITEM_FIXTURE", content=self._issue_content_payload(712)),
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

        second_client = FakeGithubProjectApiClient(
            pages={
                (None, 1): {
                    "project_title": "Daedalus Maintenance",
                    "project_url": None,
                    "items": [
                        self._project_item_payload("ITEM_FIXTURE", content=self._issue_content_payload(712)),
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
            github_client=second_client,
        )

        self.assertEqual(len(store.rows_by_key), 1)

    def _project_item_payload(
        self,
        node_id: str,
        *,
        content: dict | None,
        field_values: list[dict] | None = None,
        updated_at: str = "2026-03-28T12:00:00Z",
    ) -> dict:
        return {
            "id": node_id,
            "updatedAt": updated_at,
            "isArchived": False,
            "fieldValues": {
                "pageInfo": {
                    "hasNextPage": False,
                    "endCursor": None,
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
