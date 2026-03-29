from __future__ import annotations

import tempfile
import time
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import patch

from agentic_kb.ingest import docs


class FakeEmbeddingClient:
    def __init__(self):
        self.calls: list[list[str]] = []

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        self.calls.append(list(texts))
        return [[float(index)] * 384 for index, _ in enumerate(texts, start=1)]


class ContextLimitFakeEmbeddingClient:
    def __init__(self, *, max_chars: int):
        self.max_chars = max_chars
        self.calls: list[list[str]] = []

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        self.calls.append(list(texts))
        for text in texts:
            if len(text) > self.max_chars:
                raise docs.EmbeddingResponseError(
                    "Ollama embed request failed with HTTP 400 via http://ollama:11434/api/embed: "
                    "the input length exceeds the context length"
                )
        return [[float(len(text))] * 384 for text in texts]


class FailingReplaceDocsStore(docs.InMemoryDocsStore):
    def __init__(self):
        super().__init__()
        self.fail_on_upsert = False

    def upsert_documents(self, documents):
        if self.fail_on_upsert:
            raise RuntimeError("simulated replace failure")
        return super().upsert_documents(documents)

    def replace_documents_for_paths(self, source_paths, documents):
        self.fail_on_upsert = True
        try:
            return super().replace_documents_for_paths(source_paths, documents)
        finally:
            self.fail_on_upsert = False


class DocsIngestTests(unittest.TestCase):
    def test_plan_docs_updates_skips_unchanged_paths_by_chunk_hash_before_embedding(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "README.md", "# Title\n\nBody\n")
            store = docs.InMemoryDocsStore()
            store.upsert_documents(
                docs.prepare_documents(
                    workspace,
                    source_paths=["README.md"],
                    embedding_client=FakeEmbeddingClient(),
                    repo_commit_hash="baseline-commit",
                )
            )

            plan = docs.plan_docs_updates(
                workspace,
                docs_store=store,
                source_paths=["README.md"],
                repo_commit_hash="head-commit",
            )

        self.assertEqual(plan.candidate_paths, ("README.md",))
        self.assertEqual(plan.updated_paths, ())
        self.assertEqual(plan.skipped_paths, ("README.md",))
        self.assertEqual(plan.drafts, ())

    def test_discover_docs_source_paths_matches_allowlist_only(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "AGENTS.md", "# Agents\n")
            self._write_file(workspace / "CLAUDE.md", "# Claude\n")
            self._write_file(workspace / "README.md", "# Root\n")
            self._write_file(workspace / ".agent/readme.md", "# Agent Index\n")
            self._write_file(workspace / ".agent/workflows/build.md", "# Build\n")
            self._write_file(workspace / ".agent/skills/example/SKILL.md", "# Skill\n")
            self._write_file(workspace / ".agent/skills/example/reference/notes.md", "# Notes\n")
            self._write_file(workspace / ".agent/SOPs/readme.md", "# SOPs\n")
            self._write_file(workspace / ".agent/plans/agentic/task-plans/task-301.md", "# Task Plan\n")
            self._write_file(workspace / "tests/README.md", "# Tests\n")
            self._write_file(workspace / "tests/news/README.md", "# News\n")
            self._write_file(workspace / "installers/README.md", "# Installers\n")
            self._write_file(workspace / "installers/icons/README.md", "# Icons\n")
            self._write_file(workspace / "source/renderer/app/themes/README.md", "# Themes\n")
            self._write_file(workspace / ".agent/plans/agentic/knowledge-base-platform-tasks.json", "{}\n")
            self._write_file(workspace / "agentic/README.md", "# Excluded\n")
            self._write_file(workspace / "docs/notes.md", "# Excluded\n")
            self._write_file(workspace / "docker-compose.agentic.yml", "services:\n")

            source_paths = docs.discover_docs_source_paths(workspace)

        self.assertEqual(
            source_paths,
            [
                ".agent/SOPs/readme.md",
                ".agent/plans/agentic/task-plans/task-301.md",
                ".agent/readme.md",
                ".agent/skills/example/SKILL.md",
                ".agent/skills/example/reference/notes.md",
                ".agent/workflows/build.md",
                "AGENTS.md",
                "CLAUDE.md",
                "README.md",
                "installers/README.md",
                "installers/icons/README.md",
                "source/renderer/app/themes/README.md",
                "tests/README.md",
                "tests/news/README.md",
            ],
        )

    def test_prepare_documents_shapes_rows_with_normalized_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            workflow_path = workspace / ".agent/workflows/agentic-kb.md"
            self._write_file(
                workflow_path,
                "---\n"
                "description: Example workflow\n"
                "---\n\n"
                "# Agentic Knowledge Base Workflow\n\n"
                "## Usage\n"
                "Use this workflow to exercise the docs ingestor.\n",
            )

            embedding_client = FakeEmbeddingClient()
            prepared = docs.prepare_documents(
                workspace,
                source_paths=[".agent/workflows/agentic-kb.md"],
                embedding_client=embedding_client,
                repo_commit_hash="abc123",
            )

        self.assertEqual(len(prepared), 2)
        intro_document, section_document = prepared
        self.assertEqual(intro_document.id, "docs:.agent/workflows/agentic-kb.md#0")
        self.assertEqual(section_document.id, "docs:.agent/workflows/agentic-kb.md#1")
        document = intro_document
        self.assertEqual(document.source_domain, "docs")
        self.assertEqual(document.doc_kind, "workflow")
        self.assertEqual(document.source_path, ".agent/workflows/agentic-kb.md")
        self.assertEqual(document.title, "Agentic Knowledge Base Workflow")
        self.assertEqual(document.chunk_index, 0)
        self.assertEqual(document.section_title, None)
        self.assertEqual(document.subsection_title, None)
        self.assertEqual(document.heading_path, [])
        self.assertEqual(document.repo_commit_hash, "abc123")
        self.assertEqual(document.metadata["relative_path"], ".agent/workflows/agentic-kb.md")
        self.assertEqual(document.metadata["source_group"], "agent")
        self.assertEqual(document.metadata["title_source"], "h1")
        self.assertTrue(document.metadata["title_from_h1"])
        self.assertEqual(document.metadata["workflow_description"], "Example workflow")
        self.assertIn("description: Example workflow", document.preview_text)
        self.assertEqual(len(document.embedding), 384)
        self.assertEqual(document.embedding[0], 1.0)
        self.assertIsNotNone(document.source_updated_at.tzinfo)
        self.assertEqual(section_document.heading_path, ["Usage"])
        self.assertEqual(section_document.section_title, "Usage")
        self.assertEqual(section_document.subsection_title, None)
        self.assertEqual(
            ["".join(call) for call in embedding_client.calls],
            [document.content, section_document.content],
        )

    def test_prepare_documents_falls_back_to_basename_title_without_h1(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            readme_path = workspace / "installers/README.md"
            self._write_file(readme_path, "Installers and packaging notes.\n")

            prepared = docs.prepare_documents(
                workspace,
                source_paths=["installers/README.md"],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="def456",
            )

        document = prepared[0]
        self.assertEqual(document.title, "README")
        self.assertEqual(document.doc_kind, "readme")
        self.assertEqual(document.metadata["title_source"], "basename")
        self.assertFalse(document.metadata["title_from_h1"])
        self.assertEqual(
            document.content_hash,
            docs.deterministic_content_hash("installers/README.md", "Installers and packaging notes.\n", chunk_index=0),
        )

    def test_prepare_documents_extracts_canonical_task_plan_metadata(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-303.md",
                "# Task Plan: task-303 Extract structured plan and workflow metadata\n\n"
                "- Task ID: `task-303`\n"
                "- Title: `Extract structured plan and workflow metadata`\n"
                "- Planning Status: `approved`\n"
                "- Build Status: `completed`\n\n"
                "## Scope\n\n"
                "- Keep this narrow.\n",
            )

            prepared = docs.prepare_documents(
                workspace,
                source_paths=[".agent/plans/agentic/task-plans/task-303.md"],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="task-plan-metadata",
            )

        document = prepared[0]
        self.assertEqual(document.doc_kind, "plan")
        self.assertEqual(document.metadata["plan_type"], "canonical_task_plan")
        self.assertEqual(document.metadata["task_id"], "task-303")
        self.assertEqual(document.metadata["title"], "Extract structured plan and workflow metadata")
        self.assertEqual(document.metadata["planning_status"], "approved")
        self.assertEqual(document.metadata["build_status"], "completed")

    def test_prepare_documents_does_not_misclassify_plan_review_or_impl_review_logs(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-303-plan-review.md",
                "- Task ID: `task-303`\n- Planning Status: `approved`\n",
            )
            self._write_file(
                workspace / ".agent/plans/agentic/task-plans/task-303-impl-review.md",
                "Implementation: Iteration 1\n",
            )

            prepared = docs.prepare_documents(
                workspace,
                source_paths=[
                    ".agent/plans/agentic/task-plans/task-303-plan-review.md",
                    ".agent/plans/agentic/task-plans/task-303-impl-review.md",
                ],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="review-logs",
            )

        review_rows = {document.source_path: document for document in prepared}
        self.assertEqual(
            review_rows[".agent/plans/agentic/task-plans/task-303-plan-review.md"].metadata["plan_type"],
            "plan_review_log",
        )
        self.assertEqual(
            review_rows[".agent/plans/agentic/task-plans/task-303-impl-review.md"].metadata["plan_type"],
            "implementation_review_log",
        )
        self.assertNotIn(
            "task_id",
            review_rows[".agent/plans/agentic/task-plans/task-303-plan-review.md"].metadata,
        )
        self.assertNotIn(
            "planning_status",
            review_rows[".agent/plans/agentic/task-plans/task-303-impl-review.md"].metadata,
        )

    def test_prepare_documents_chunks_markdown_headings_with_intro_and_nested_paths(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / "README.md",
                "# Title\n\n"
                "Intro paragraph.\n\n"
                "## First Section\n"
                "Alpha body.\n\n"
                "### Deep Detail\n"
                "Nested body.\n\n"
                "## Second Section\n"
                "Omega body.\n",
            )

            prepared = docs.prepare_documents(
                workspace,
                source_paths=["README.md"],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="chunked-commit",
            )

        self.assertEqual([document.chunk_index for document in prepared], [0, 1, 2, 3])
        self.assertEqual([document.id for document in prepared], [
            "docs:README.md#0",
            "docs:README.md#1",
            "docs:README.md#2",
            "docs:README.md#3",
        ])
        self.assertEqual(prepared[0].heading_path, [])
        self.assertEqual(prepared[0].content, "# Title\n\nIntro paragraph.")
        self.assertEqual(prepared[1].heading_path, ["First Section"])
        self.assertEqual(prepared[1].section_title, "First Section")
        self.assertEqual(prepared[1].subsection_title, None)
        self.assertTrue(prepared[1].content.startswith("## First Section\n"))
        self.assertEqual(prepared[2].heading_path, ["First Section", "Deep Detail"])
        self.assertEqual(prepared[2].section_title, "First Section")
        self.assertEqual(prepared[2].subsection_title, "Deep Detail")
        self.assertEqual(prepared[3].heading_path, ["Second Section"])
        self.assertEqual(prepared[3].section_title, "Second Section")
        self.assertEqual(prepared[3].subsection_title, None)

    def test_prepare_documents_keeps_headingless_docs_as_single_row(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "README.md", "Plain intro\n\nStill no headings.\n")

            prepared = docs.prepare_documents(
                workspace,
                source_paths=["README.md"],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="single-row",
            )

        self.assertEqual(len(prepared), 1)
        self.assertEqual(prepared[0].heading_path, [])
        self.assertEqual(prepared[0].section_title, None)
        self.assertEqual(prepared[0].subsection_title, None)
        self.assertEqual(prepared[0].chunk_index, 0)

    def test_prepare_documents_ignores_atx_like_lines_inside_fenced_code_blocks(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(
                workspace / ".agent/workflows/agentic-kb.md",
                "# Agentic Knowledge Base Workflow\n\n"
                "```bash\n"
                "# Start the stack\n"
                "docker compose -f docker-compose.agentic.yml up -d\n"
                "```\n\n"
                "## Real Section\n"
                "Use this workflow after the stack is ready.\n",
            )

            prepared = docs.prepare_documents(
                workspace,
                source_paths=[".agent/workflows/agentic-kb.md"],
                embedding_client=FakeEmbeddingClient(),
                repo_commit_hash="fenced-code",
            )

        self.assertEqual(len(prepared), 2)
        self.assertEqual(prepared[0].heading_path, [])
        self.assertIn("# Start the stack", prepared[0].content)
        self.assertEqual(prepared[1].heading_path, ["Real Section"])
        self.assertEqual(prepared[1].section_title, "Real Section")
        self.assertNotIn("Start the stack", prepared[1].heading_path)

    def test_ingest_docs_upserts_changed_content_without_duplication(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            agents_path = workspace / "AGENTS.md"
            readme_path = workspace / "README.md"
            self._write_file(agents_path, "# Agents\n\nIntro\n\n## Rules\nInitial content.\n")
            self._write_file(readme_path, "# Daedalus\n\nRoot notes.\n")

            embedding_client = FakeEmbeddingClient()
            store = docs.InMemoryDocsStore()

            first_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )

            first_agents_chunk_keys = sorted(key for key in store.rows_by_key if key[0] == "AGENTS.md")
            time.sleep(0.02)
            self._write_file(agents_path, "# Agents\n\nUpdated content for rerun.\n")

            second_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

        self.assertEqual(first_result.processed_count, 3)
        self.assertEqual(second_result.processed_count, 1)
        self.assertEqual(first_result.source_paths, ("AGENTS.md", "README.md"))
        self.assertEqual(second_result.candidate_paths, ("AGENTS.md", "README.md"))
        self.assertEqual(second_result.source_paths, ("AGENTS.md",))
        self.assertEqual(second_result.updated_paths, ("AGENTS.md",))
        self.assertEqual(second_result.skipped_paths, ("README.md",))
        self.assertEqual(len(store.rows_by_key), 2)
        self.assertEqual(first_agents_chunk_keys, [("AGENTS.md", 0), ("AGENTS.md", 1)])
        second_agents_row = store.rows_by_key[("AGENTS.md", 0)]
        self.assertEqual(second_agents_row["repo_commit_hash"], "commit-b")
        self.assertIn("Updated content for rerun", second_agents_row["content"])
        self.assertNotIn(("AGENTS.md", 1), store.rows_by_key)
        self.assertEqual(len(embedding_client.calls), 4)
        self.assertEqual("".join(embedding_client.calls[0]), "# Agents\n\nIntro")
        self.assertEqual("".join(embedding_client.calls[1]), "## Rules\nInitial content.")
        self.assertEqual("".join(embedding_client.calls[2]), "# Daedalus\n\nRoot notes.")
        self.assertEqual("".join(embedding_client.calls[3]), "# Agents\n\nUpdated content for rerun.")

    def test_ingest_docs_skips_unchanged_docs_without_embedding_or_row_rewrite(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            self._write_file(workspace / "README.md", "# Title\n\nBody\n")
            store = docs.InMemoryDocsStore()

            first_embedding_client = FakeEmbeddingClient()
            first_result = docs.ingest_docs(
                workspace,
                embedding_client=first_embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )
            first_row = dict(store.rows_by_key[("README.md", 0)])
            second_embedding_client = FakeEmbeddingClient()

            second_result = docs.ingest_docs(
                workspace,
                embedding_client=second_embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

        self.assertEqual(first_result.updated_paths, ("README.md",))
        self.assertEqual(first_result.skipped_paths, ())
        self.assertEqual(second_result.candidate_paths, ("README.md",))
        self.assertEqual(second_result.updated_paths, ())
        self.assertEqual(second_result.skipped_paths, ("README.md",))
        self.assertEqual(second_result.processed_count, 0)
        self.assertEqual(second_embedding_client.calls, [])
        self.assertEqual(store.rows_by_key[("README.md", 0)], first_row)
        self.assertEqual(store.rows_by_key[("README.md", 0)]["repo_commit_hash"], "commit-a")

    def test_ingest_docs_backfills_missing_structured_metadata_once_then_skips(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            source_path = ".agent/plans/agentic/task-plans/task-303.md"
            file_content = (
                "- Task ID: `task-303`\n"
                "- Title: `Extract structured plan and workflow metadata`\n"
                "- Planning Status: `approved`\n"
                "- Build Status: `completed`\n"
            )
            self._write_file(
                workspace / source_path,
                file_content,
            )
            file_size_bytes = (workspace / source_path).stat().st_size
            store = docs.InMemoryDocsStore()
            legacy_document = docs.PreparedDocument(
                id=f"docs:{source_path}#0",
                source_domain="docs",
                doc_kind="plan",
                source_path=source_path,
                title="Task 303",
                section_title=None,
                subsection_title=None,
                heading_path=[],
                chunk_index=0,
                content=file_content,
                preview_text="legacy preview",
                content_hash=docs.deterministic_content_hash(
                    source_path,
                    file_content,
                    chunk_index=0,
                ),
                repo_commit_hash="legacy-commit",
                source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
                embedding=[1.0] * 384,
                metadata={
                    "relative_path": source_path,
                    "file_size_bytes": file_size_bytes,
                    "source_group": "agent",
                    "title_source": "basename",
                    "title_from_h1": False,
                },
            )
            store.upsert_documents([legacy_document])

            backfill_embedding_client = FakeEmbeddingClient()
            backfill_result = docs.ingest_docs(
                workspace,
                embedding_client=backfill_embedding_client,
                docs_store=store,
                repo_commit_hash="backfill-commit",
            )
            rewritten_row = dict(store.rows_by_key[(source_path, 0)])

            skip_embedding_client = FakeEmbeddingClient()
            skip_result = docs.ingest_docs(
                workspace,
                embedding_client=skip_embedding_client,
                docs_store=store,
                repo_commit_hash="skip-commit",
            )

        self.assertEqual(backfill_result.updated_paths, (source_path,))
        self.assertEqual(backfill_result.skipped_paths, ())
        self.assertTrue(backfill_embedding_client.calls)
        self.assertEqual(rewritten_row["repo_commit_hash"], "backfill-commit")
        self.assertEqual(rewritten_row["metadata"]["task_id"], "task-303")
        self.assertEqual(rewritten_row["metadata"]["planning_status"], "approved")
        self.assertEqual(rewritten_row["metadata"]["build_status"], "completed")
        self.assertEqual(rewritten_row["metadata"]["plan_type"], "canonical_task_plan")
        self.assertEqual(skip_result.updated_paths, ())
        self.assertEqual(skip_result.skipped_paths, (source_path,))
        self.assertEqual(skip_embedding_client.calls, [])
        self.assertEqual(store.rows_by_key[(source_path, 0)], rewritten_row)

    def test_replace_documents_for_paths_rolls_back_in_memory_on_failure(self):
        store = FailingReplaceDocsStore()
        baseline_document = docs.PreparedDocument(
            id="docs:README.md#0",
            source_domain="docs",
            doc_kind="readme",
            source_path="README.md",
            title="README",
            section_title=None,
            subsection_title=None,
            heading_path=[],
            chunk_index=0,
            content="baseline",
            preview_text="baseline",
            content_hash="hash-a",
            repo_commit_hash="commit-a",
            source_updated_at=datetime(2026, 3, 29, 9, 0, tzinfo=timezone.utc),
            embedding=[1.0] * 384,
            metadata={},
        )
        store.upsert_documents([baseline_document])

        replacement_document = docs.PreparedDocument(
            id="docs:README.md#0",
            source_domain="docs",
            doc_kind="readme",
            source_path="README.md",
            title="README",
            section_title=None,
            subsection_title=None,
            heading_path=[],
            chunk_index=0,
            content="replacement",
            preview_text="replacement",
            content_hash="hash-b",
            repo_commit_hash="commit-b",
            source_updated_at=datetime(2026, 3, 29, 10, 0, tzinfo=timezone.utc),
            embedding=[2.0] * 384,
            metadata={},
        )

        with self.assertRaisesRegex(RuntimeError, "simulated replace failure"):
            store.replace_documents_for_paths(("README.md",), [replacement_document])

        self.assertEqual(store.rows_by_key[("README.md", 0)]["content"], "baseline")

    def test_helper_contracts_cover_classification_preview_and_path_normalization(self):
        self.assertEqual(docs.classify_doc_kind(".agent/readme.md"), "agent_index")
        self.assertEqual(docs.classify_doc_kind("AGENTS.md"), "agent_instruction")
        self.assertEqual(docs.classify_doc_kind(".agent/SOPs/readme.md"), "sop")
        self.assertEqual(docs.classify_doc_kind(".agent/plans/agentic/task-plans/task-301.md"), "plan")
        self.assertEqual(docs.classify_doc_kind("tests/README.md"), "readme")
        self.assertEqual(
            docs.deterministic_document_id(".agent/workflows/agentic-kb.md"),
            "docs:.agent/workflows/agentic-kb.md#0",
        )
        self.assertEqual(
            docs.normalize_source_path("/workspace/repo", "/workspace/repo/.agent/workflows/agentic-kb.md"),
            ".agent/workflows/agentic-kb.md",
        )
        self.assertEqual(
            docs.build_preview_text("\n\nFirst line\n\nSecond line\n"),
            "First line Second line",
        )
        self.assertTrue(docs.build_preview_text("word " * 100).endswith("..."))
        self.assertEqual(
            docs._extract_plan_type(".agent/plans/agentic/task-plans/task-303.md"),
            "canonical_task_plan",
        )
        self.assertEqual(
            docs._extract_plan_type(".agent/plans/agentic/task-plans/task-303-plan-review.md"),
            "plan_review_log",
        )
        self.assertEqual(
            docs._extract_plan_type(".agent/plans/agentic/task-plans/task-303-impl-review.md"),
            "implementation_review_log",
        )

    def test_embedding_batches_preserve_input_order(self):
        embedding_client = FakeEmbeddingClient()

        vectors = docs._embed_texts_in_batches(
            ["alpha", "beta", "gamma"],
            embedding_client=embedding_client,
            max_batch_texts=2,
            max_batch_chars=100,
        )

        self.assertEqual(embedding_client.calls, [["alpha", "beta"], ["gamma"]])
        self.assertEqual(len(vectors), 3)
        self.assertEqual(vectors[0][0], 1.0)
        self.assertEqual(vectors[1][0], 2.0)
        self.assertEqual(vectors[2][0], 1.0)

    def test_split_text_for_embedding_prefers_newline_boundaries(self):
        segments = docs._split_text_for_embedding(
            "alpha line\nbeta line\ngamma line\n",
            max_chars=12,
        )

        self.assertEqual(segments, ["alpha line", "beta line", "gamma line"])

    def test_aggregate_segment_embeddings_uses_length_weighting(self):
        vector = docs._aggregate_segment_embeddings(
            ["aa", "bbbb"],
            [[1.0] * 384, [3.0] * 384],
        )

        self.assertEqual(len(vector), 384)
        self.assertAlmostEqual(vector[0], (2.0 + 12.0) / 6.0)

    def test_embed_document_content_segments_oversized_docs_without_splitting_rows(self):
        embedding_client = ContextLimitFakeEmbeddingClient(max_chars=700)
        content = "\n".join(f"line {index} with enough content to force segmentation" for index in range(120))

        vector = docs._embed_document_content(content, embedding_client=embedding_client)

        self.assertEqual(len(vector), 384)
        self.assertGreater(len(embedding_client.calls), 1)
        self.assertTrue(all(len(text) <= 700 for call in embedding_client.calls for text in call))
        self.assertGreater(vector[0], 0.0)

    def test_prepare_documents_returns_single_row_for_segmented_embedding_content(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            content = "# Large Doc\n\n" + ("paragraph with useful content\n" * 250)
            self._write_file(workspace / "README.md", content)

            prepared = docs.prepare_documents(
                workspace,
                source_paths=["README.md"],
                embedding_client=ContextLimitFakeEmbeddingClient(max_chars=700),
                repo_commit_hash="segmented-commit",
            )

        self.assertEqual(len(prepared), 1)
        self.assertEqual(prepared[0].source_path, "README.md")
        self.assertEqual(prepared[0].chunk_index, 0)
        self.assertEqual(prepared[0].content, content)

    def test_repo_commit_hash_falls_back_to_git_metadata_when_git_binary_missing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            git_dir = workspace / ".git"
            refs_dir = git_dir / "refs/heads"
            refs_dir.mkdir(parents=True, exist_ok=True)
            (git_dir / "HEAD").write_text("ref: refs/heads/main\n", encoding="utf-8")
            (refs_dir / "main").write_text("abc123deadbeef\n", encoding="utf-8")

            with patch("agentic_kb.ingest.docs.subprocess.run", side_effect=FileNotFoundError()):
                commit_hash = docs.get_repo_commit_hash(workspace)

        self.assertEqual(commit_hash, "abc123deadbeef")

    def _write_file(self, path: Path, content: str) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")


if __name__ == "__main__":
    unittest.main()
