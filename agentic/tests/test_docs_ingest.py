from __future__ import annotations

import tempfile
import time
import unittest
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


class DocsIngestTests(unittest.TestCase):
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
                "Use this workflow to exercise the docs ingestor.\n",
            )

            embedding_client = FakeEmbeddingClient()
            prepared = docs.prepare_documents(
                workspace,
                source_paths=[".agent/workflows/agentic-kb.md"],
                embedding_client=embedding_client,
                repo_commit_hash="abc123",
            )

        self.assertEqual(len(prepared), 1)
        document = prepared[0]
        self.assertEqual(document.id, "docs:.agent/workflows/agentic-kb.md#0")
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
        self.assertIn("Use this workflow", document.preview_text)
        self.assertEqual(len(document.embedding), 384)
        self.assertEqual(document.embedding[0], 1.0)
        self.assertIsNotNone(document.source_updated_at.tzinfo)
        self.assertEqual(["".join(call) for call in embedding_client.calls], [document.content.strip()])

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
            docs.deterministic_content_hash("installers/README.md", "Installers and packaging notes.\n"),
        )

    def test_ingest_docs_upserts_changed_content_without_duplication(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            agents_path = workspace / "AGENTS.md"
            readme_path = workspace / "README.md"
            self._write_file(agents_path, "# Agents\n\nInitial content.\n")
            self._write_file(readme_path, "# Daedalus\n\nRoot notes.\n")

            embedding_client = FakeEmbeddingClient()
            store = docs.InMemoryDocsStore()

            first_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-a",
            )

            first_agents_row = dict(store.rows_by_key[("AGENTS.md", 0)])
            time.sleep(0.02)
            self._write_file(agents_path, "# Agents\n\nUpdated content for rerun.\n")

            second_result = docs.ingest_docs(
                workspace,
                embedding_client=embedding_client,
                docs_store=store,
                repo_commit_hash="commit-b",
            )

        self.assertEqual(first_result.processed_count, 2)
        self.assertEqual(second_result.processed_count, 2)
        self.assertEqual(first_result.source_paths, ("AGENTS.md", "README.md"))
        self.assertEqual(second_result.source_paths, ("AGENTS.md", "README.md"))
        self.assertEqual(len(store.rows_by_key), 2)
        second_agents_row = store.rows_by_key[("AGENTS.md", 0)]
        self.assertEqual(second_agents_row["repo_commit_hash"], "commit-b")
        self.assertIn("Updated content for rerun", second_agents_row["content"])
        self.assertNotEqual(second_agents_row["content_hash"], first_agents_row["content_hash"])
        self.assertGreater(second_agents_row["updated_at_token"], first_agents_row["updated_at_token"])
        self.assertEqual(len(embedding_client.calls), 4)
        self.assertEqual("".join(embedding_client.calls[0]), "# Agents\n\nInitial content.")
        self.assertEqual("".join(embedding_client.calls[1]), "# Daedalus\n\nRoot notes.")
        self.assertEqual("".join(embedding_client.calls[2]), "# Agents\n\nUpdated content for rerun.")
        self.assertEqual("".join(embedding_client.calls[3]), "# Daedalus\n\nRoot notes.")

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
