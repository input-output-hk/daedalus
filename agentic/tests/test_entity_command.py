from __future__ import annotations

import io
import json
import unittest
from argparse import Namespace
from contextlib import redirect_stderr, redirect_stdout
from unittest.mock import patch

from agentic_kb.commands import entity
from agentic_kb.search import SearchEntityType, SearchValidationError


class EntityCommandTests(unittest.TestCase):
    @patch("agentic_kb.commands.entity.fetch_entity_row")
    @patch("agentic_kb.commands.entity.AgenticConfig.from_env")
    def test_get_entity_from_args_returns_generic_payload(self, from_env, fetch_entity_row):
        from_env.return_value = type("Config", (), {"database_url": "postgresql://db"})()
        fetch_entity_row.return_value = {"id": "docs:1", "title": "Doc 1", "content": "Full body"}
        args = Namespace(entity_type="documents", id="docs:1", json=False)

        payload = entity.get_entity_from_args(args)

        self.assertEqual(payload["entity_type"], SearchEntityType.DOCUMENTS)
        self.assertEqual(payload["id"], "docs:1")
        self.assertEqual(payload["row"]["title"], "Doc 1")

    @patch("agentic_kb.commands.entity.AgenticConfig.from_env")
    def test_get_entity_from_args_validates_unknown_entity_type_before_query(self, from_env):
        from_env.return_value = type("Config", (), {"database_url": "postgresql://db"})()
        args = Namespace(entity_type="bogus", id="docs:1", json=False)

        with self.assertRaises(SearchValidationError):
            entity.get_entity_from_args(args)

    @patch("agentic_kb.commands.entity.AgenticConfig.from_env")
    def test_get_entity_from_args_invalid_entity_type_wins_when_database_url_missing(self, from_env):
        from_env.return_value = type("Config", (), {"database_url": None})()
        args = Namespace(entity_type="bogus", id="docs:1", json=False)

        with self.assertRaisesRegex(SearchValidationError, "Unsupported entity_type value"):
            entity.get_entity_from_args(args)

    @patch("agentic_kb.commands.entity.AgenticConfig.from_env")
    def test_get_entity_from_args_requires_database_url_after_entity_type_validation(self, from_env):
        from_env.return_value = type("Config", (), {"database_url": None})()
        args = Namespace(entity_type="documents", id="docs:1", json=False)

        with self.assertRaisesRegex(SearchValidationError, "DATABASE_URL is required"):
            entity.get_entity_from_args(args)

    @patch("agentic_kb.commands.entity.fetch_entity_row", return_value=None)
    @patch("agentic_kb.commands.entity.AgenticConfig.from_env")
    def test_get_entity_from_args_raises_not_found_for_missing_row(self, from_env, _fetch_entity_row):
        from_env.return_value = type("Config", (), {"database_url": "postgresql://db"})()
        args = Namespace(entity_type="documents", id="docs:404", json=True)

        with self.assertRaisesRegex(entity.EntityCommandError, "docs:404"):
            entity.get_entity_from_args(args)

    def test_print_entity_text_renders_sorted_rows(self):
        payload = {
            "entity_type": SearchEntityType.DOCUMENTS,
            "id": "docs:1",
            "table_name": "agentic.kb_documents",
            "row": {"id": "docs:1", "title": "Doc 1", "content": "Full body"},
        }

        stdout = io.StringIO()
        with redirect_stdout(stdout):
            entity.print_entity_text(payload)

        output = stdout.getvalue()
        self.assertIn("entity: documents docs:1", output)
        self.assertIn("content: Full body", output)
        self.assertIn("title: Doc 1", output)

    @patch("agentic_kb.commands.entity.get_entity_from_args")
    def test_run_entity_get_json_success_emits_only_json(self, get_entity_from_args):
        get_entity_from_args.return_value = {
            "entity_type": SearchEntityType.DOCUMENTS,
            "id": "docs:1",
            "table_name": "agentic.kb_documents",
            "row": {"id": "docs:1", "title": "Doc 1"},
        }
        args = Namespace(entity_type="documents", id="docs:1", json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            exit_code = entity.run_entity_get(args)

        self.assertEqual(exit_code, 0)
        self.assertEqual(stderr.getvalue(), "")
        self.assertEqual(json.loads(stdout.getvalue()), {
            "entity_type": "documents",
            "id": "docs:1",
            "table_name": "agentic.kb_documents",
            "row": {"id": "docs:1", "title": "Doc 1"},
        })

    @patch("agentic_kb.commands.entity.get_entity_from_args", side_effect=SearchValidationError("unsupported"))
    def test_run_entity_get_invalid_type_returns_exit_code_two(self, _get_entity_from_args):
        args = Namespace(entity_type="bogus", id="x", json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            exit_code = entity.run_entity_get(args)

        self.assertEqual(exit_code, 2)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("entity get failed: unsupported", stderr.getvalue())

    @patch("agentic_kb.commands.entity.get_entity_from_args", side_effect=entity.EntityCommandError("documents entity not found for id 'docs:404'"))
    def test_run_entity_get_not_found_returns_exit_code_four_and_no_stdout(self, _get_entity_from_args):
        args = Namespace(entity_type="documents", id="docs:404", json=True)

        stdout = io.StringIO()
        stderr = io.StringIO()
        with redirect_stdout(stdout), redirect_stderr(stderr):
            exit_code = entity.run_entity_get(args)

        self.assertEqual(exit_code, 4)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("documents entity not found", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
