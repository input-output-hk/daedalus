from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Mapping, Sequence

import yaml

from agentic_kb.search import SearchMode, SearchRequest, SearchValidationError, select_search_entity_configs


FIXTURE_FILE_PATH = Path(__file__).resolve().parents[1] / "config" / "search-fixtures.yaml"
SCHEMA_DIR = Path(__file__).resolve().parents[1] / "schema"


class SearchFixtureValidationError(ValueError):
    pass


@dataclass(frozen=True)
class SearchFixtureExpectations:
    top_hit: str | None = None
    ordered_prefix: tuple[str, ...] = ()
    contains_unordered: tuple[str, ...] = ()


@dataclass(frozen=True)
class SearchQualityFixture:
    fixture_id: str
    query_text: str
    mode: SearchMode
    limit: int
    filters: Mapping[str, Any]
    query_embedding_key: str | None
    expectations: SearchFixtureExpectations

    def build_request(self) -> SearchRequest:
        return SearchRequest(
            query_text=self.query_text,
            limit=self.limit,
            mode=self.mode,
            filters=dict(self.filters),
        )


def load_search_quality_fixtures(path: Path | None = None) -> tuple[SearchQualityFixture, ...]:
    fixture_path = path or FIXTURE_FILE_PATH
    raw_payload = yaml.safe_load(fixture_path.read_text(encoding="utf-8"))
    if not isinstance(raw_payload, dict):
        raise SearchFixtureValidationError("search fixtures file must contain a top-level mapping")

    version = raw_payload.get("version")
    if version != 1:
        raise SearchFixtureValidationError("search fixtures file must declare version: 1")

    raw_fixtures = raw_payload.get("fixtures")
    if not isinstance(raw_fixtures, list) or not raw_fixtures:
        raise SearchFixtureValidationError("search fixtures file must define a non-empty fixtures list")

    fixtures: list[SearchQualityFixture] = []
    seen_ids: set[str] = set()
    for index, raw_fixture in enumerate(raw_fixtures, start=1):
        fixture = _validate_fixture(raw_fixture, index=index)
        if fixture.fixture_id in seen_ids:
            raise SearchFixtureValidationError(
                f"duplicate search fixture id: {fixture.fixture_id}"
            )
        seen_ids.add(fixture.fixture_id)
        fixtures.append(fixture)
    return tuple(fixtures)


def fixture_query_embedding(fixture: SearchQualityFixture) -> list[float] | None:
    if fixture.query_embedding_key is None:
        return None
    try:
        return list(FIXTURE_QUERY_EMBEDDINGS[fixture.query_embedding_key])
    except KeyError as error:
        raise SearchFixtureValidationError(
            f"search fixture {fixture.fixture_id!r} references unknown query_embedding_key {fixture.query_embedding_key!r}"
        ) from error


def assert_fixture_result(testcase, fixture: SearchQualityFixture, result) -> None:
    hit_ids = [hit.id for hit in result.hits]
    expectations = fixture.expectations
    if expectations.top_hit is not None:
        testcase.assertGreater(
            len(hit_ids),
            0,
            msg=f"fixture {fixture.fixture_id} expected top hit {expectations.top_hit!r} but returned no hits",
        )
        testcase.assertEqual(hit_ids[0], expectations.top_hit)
    if expectations.ordered_prefix:
        testcase.assertEqual(
            hit_ids[: len(expectations.ordered_prefix)],
            list(expectations.ordered_prefix),
        )
    if expectations.contains_unordered:
        testcase.assertTrue(
            set(expectations.contains_unordered).issubset(set(hit_ids)),
            msg=(
                f"fixture {fixture.fixture_id} expected hits {list(expectations.contains_unordered)!r} "
                f"within {hit_ids!r}"
            ),
        )


def bootstrap_database(connection) -> None:
    init_sql = _sanitized_sql(SCHEMA_DIR / "init.sql")
    create_indexes_sql = (SCHEMA_DIR / "create_indexes.sql").read_text(encoding="utf-8")
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute("DROP SCHEMA IF EXISTS agentic CASCADE")
            cursor.execute(init_sql)
            cursor.execute(create_indexes_sql)


def reset_and_seed_search_quality_corpus(connection) -> None:
    with connection.transaction():
        with connection.cursor() as cursor:
            cursor.execute(
                "TRUNCATE TABLE agentic.kb_code_chunks, agentic.kb_github_issues, agentic.kb_project_items, agentic.kb_documents CASCADE"
            )
            _seed_documents(cursor)
            _seed_code_chunks(cursor)
            _seed_github_issues(cursor)
            _seed_project_items(cursor)


def _validate_fixture(raw_fixture: Any, *, index: int) -> SearchQualityFixture:
    if not isinstance(raw_fixture, dict):
        raise SearchFixtureValidationError(f"search fixture #{index} must be a mapping")

    fixture_id = _require_non_empty_string(raw_fixture.get("id"), field_name="id", fixture_label=f"#{index}")
    query_text = _require_non_empty_string(
        raw_fixture.get("query_text"),
        field_name="query_text",
        fixture_label=fixture_id,
    )
    limit = raw_fixture.get("limit", 10)
    if isinstance(limit, bool) or not isinstance(limit, int) or limit <= 0:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} must use a positive integer limit"
        )

    raw_filters = raw_fixture.get("filters", {})
    if not isinstance(raw_filters, dict):
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} filters must be a mapping"
        )
    filters = _validate_filters(raw_filters, fixture_id=fixture_id)

    raw_mode = raw_fixture.get("mode")
    try:
        mode = SearchMode(raw_mode)
    except Exception as error:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} has unsupported mode {raw_mode!r}"
        ) from error

    raw_embedding_key = raw_fixture.get("query_embedding_key")
    query_embedding_key = None
    if raw_embedding_key is not None:
        query_embedding_key = _require_non_empty_string(
            raw_embedding_key,
            field_name="query_embedding_key",
            fixture_label=fixture_id,
        )

    if mode == SearchMode.BM25 and query_embedding_key is not None:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} must not set query_embedding_key for bm25 mode"
        )
    if mode in {SearchMode.VECTOR, SearchMode.HYBRID} and query_embedding_key is None:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} must set query_embedding_key for {mode.value} mode"
        )
    if query_embedding_key is not None and query_embedding_key not in FIXTURE_QUERY_EMBEDDINGS:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} references unknown query_embedding_key {query_embedding_key!r}"
        )

    expectations = _validate_expectations(raw_fixture.get("expectations"), fixture_id=fixture_id)
    fixture = SearchQualityFixture(
        fixture_id=fixture_id,
        query_text=query_text,
        mode=mode,
        limit=limit,
        filters=filters,
        query_embedding_key=query_embedding_key,
        expectations=expectations,
    )
    _validate_fixture_search_contract(fixture)
    return fixture


def _validate_filters(raw_filters: Mapping[str, Any], *, fixture_id: str) -> dict[str, Any]:
    normalized: dict[str, Any] = {}
    for key, value in raw_filters.items():
        if not isinstance(key, str) or not key.strip():
            raise SearchFixtureValidationError(
                f"search fixture {fixture_id!r} uses an invalid filter key {key!r}"
            )
        normalized_key = key.strip()
        if normalized_key == "entity_type":
            normalized[normalized_key] = _validate_entity_type_filter(value, fixture_id=fixture_id)
            continue
        if isinstance(value, str):
            stripped = value.strip()
            if not stripped:
                raise SearchFixtureValidationError(
                    f"search fixture {fixture_id!r} filter {normalized_key!r} must not be empty"
                )
            normalized[normalized_key] = stripped
            continue
        if isinstance(value, bool) or not isinstance(value, int):
            raise SearchFixtureValidationError(
                f"search fixture {fixture_id!r} filter {normalized_key!r} must be a string or integer"
            )
        normalized[normalized_key] = value
    return normalized


def _validate_entity_type_filter(value: Any, *, fixture_id: str) -> str | list[str]:
    if isinstance(value, str):
        normalized = value.strip()
        if not normalized:
            raise SearchFixtureValidationError(
                f"search fixture {fixture_id!r} entity_type filter must not be empty"
            )
        return normalized
    if isinstance(value, list) and value:
        normalized_values: list[str] = []
        for item in value:
            if not isinstance(item, str) or not item.strip():
                raise SearchFixtureValidationError(
                    f"search fixture {fixture_id!r} entity_type list must contain only non-empty strings"
                )
            normalized_values.append(item.strip())
        return normalized_values
    raise SearchFixtureValidationError(
        f"search fixture {fixture_id!r} entity_type filter must be a string or non-empty list of strings"
    )


def _validate_expectations(raw_expectations: Any, *, fixture_id: str) -> SearchFixtureExpectations:
    if not isinstance(raw_expectations, dict):
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} expectations must be a mapping"
        )

    supported_keys = {"top_hit", "ordered_prefix", "contains_unordered"}
    unknown_keys = sorted(key for key in raw_expectations if key not in supported_keys)
    if unknown_keys:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} uses unsupported expectation keys: {', '.join(unknown_keys)}"
        )

    top_hit = raw_expectations.get("top_hit")
    if top_hit is not None:
        top_hit = _require_non_empty_string(top_hit, field_name="top_hit", fixture_label=fixture_id)

    ordered_prefix = _normalize_expectation_list(
        raw_expectations.get("ordered_prefix", ()),
        field_name="ordered_prefix",
        fixture_label=fixture_id,
    )
    contains_unordered = _normalize_expectation_list(
        raw_expectations.get("contains_unordered", ()),
        field_name="contains_unordered",
        fixture_label=fixture_id,
    )

    if top_hit is None and not ordered_prefix and not contains_unordered:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} must define at least one ranking expectation"
        )
    if ordered_prefix and top_hit is not None and ordered_prefix[0] != top_hit:
        raise SearchFixtureValidationError(
            f"search fixture {fixture_id!r} top_hit must match ordered_prefix[0]"
        )

    return SearchFixtureExpectations(
        top_hit=top_hit,
        ordered_prefix=ordered_prefix,
        contains_unordered=contains_unordered,
    )


def _normalize_expectation_list(raw_value: Any, *, field_name: str, fixture_label: str) -> tuple[str, ...]:
    if raw_value in (None, (), []):
        return ()
    if not isinstance(raw_value, list):
        raise SearchFixtureValidationError(
            f"search fixture {fixture_label!r} {field_name} must be a list of ids"
        )
    normalized: list[str] = []
    for item in raw_value:
        normalized.append(
            _require_non_empty_string(item, field_name=field_name, fixture_label=fixture_label)
        )
    return tuple(normalized)


def _require_non_empty_string(value: Any, *, field_name: str, fixture_label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise SearchFixtureValidationError(
            f"search fixture {fixture_label!r} must define a non-empty {field_name}"
        )
    return value.strip()


def _validate_fixture_search_contract(fixture: SearchQualityFixture) -> None:
    try:
        request = fixture.build_request()
        select_search_entity_configs(request.filters)
    except SearchValidationError as error:
        raise SearchFixtureValidationError(
            f"search fixture {fixture.fixture_id!r} is incompatible with the current search registry: {error}"
        ) from error


def _seed_documents(cursor) -> None:
    cursor.executemany(
        """
        INSERT INTO agentic.kb_documents (
            id,
            source_domain,
            doc_kind,
            source_path,
            title,
            heading_path,
            chunk_index,
            content,
            preview_text,
            content_hash,
            embedding,
            metadata
        ) VALUES (
            %s,
            %s,
            %s,
            %s,
            %s,
            '[]'::jsonb,
            0,
            %s,
            %s,
            %s,
            %s::vector,
            %s::jsonb
        )
        """,
        [
            (
                "docs:task-504-plan#0",
                "docs",
                "plan",
                ".agent/plans/agentic/task-plans/task-504.md",
                "search quality fixtures plan",
                "search quality fixtures canonical plan with deterministic ranking regression coverage",
                "search quality fixtures canonical plan",
                "docs-task-504-plan-hash",
                _vector_literal(_unit_vector(1)),
                '{"task_id":"task-504","planning_status":"approved","build_status":"completed","plan_type":"canonical_task_plan"}',
            ),
            (
                "docs:workflow-search#0",
                "docs",
                "workflow",
                ".agent/workflows/agentic-kb.md",
                "search workflow guide",
                "workflow smoke checks for local search verification",
                "workflow smoke checks for local search verification",
                "docs-workflow-search-hash",
                _vector_literal(_unit_vector(8)),
                '{"workflow_description":"search workflow guide"}',
            ),
        ],
    )


def _seed_code_chunks(cursor) -> None:
    cursor.executemany(
        """
        INSERT INTO agentic.kb_code_chunks (
            id,
            repo_path,
            language,
            symbol_name,
            symbol_kind,
            parent_symbol_name,
            parent_symbol_kind,
            chunk_index,
            start_line,
            end_line,
            content,
            preview_text,
            content_hash,
            embedding,
            metadata
        ) VALUES (
            %s,
            %s,
            'python',
            %s,
            'function',
            NULL,
            NULL,
            0,
            %s,
            %s,
            %s,
            %s,
            %s,
            %s::vector,
            '{}'::jsonb
        )
        """,
        [
            (
                "code:search-quality-helper#0",
                "agentic/tests/search_quality_helpers.py",
                "load_search_quality_fixtures",
                1,
                120,
                "def load_search_quality_fixtures(): return 'fixture loader helper for deterministic search quality tests'",
                "fixture loader helper for deterministic search quality tests",
                "code-search-quality-helper-hash",
                _vector_literal(_unit_vector(3)),
            ),
            (
                "code:search-quality-test#0",
                "agentic/tests/test_search_quality_fixtures.py",
                "SearchQualityFixtureDbTests",
                1,
                200,
                "class SearchQualityFixtureDbTests: pass  # search quality regression test harness",
                "search quality regression test harness",
                "code-search-quality-test-hash",
                _vector_literal(_unit_vector(4)),
            ),
        ],
    )


def _seed_github_issues(cursor) -> None:
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
            embedding,
            metadata
        ) VALUES (
            %s,
            'DripDropz/daedalus',
            %s,
            NULL,
            %s,
            'open',
            'opencode',
            '[]'::jsonb,
            %s,
            %s,
            %s,
            '2026-03-29T00:00:00Z'::timestamptz,
            '2026-03-29T00:00:00Z'::timestamptz,
            %s::vector,
            '{}'::jsonb
        )
        """,
        [
            (
                "github:issue-search-quality",
                504,
                "Add search quality fixtures",
                "Need deterministic hybrid search quality regression fixtures for the Daedalus KB",
                "deterministic hybrid search quality regression fixtures",
                "https://example.com/issues/504",
                _vector_literal(_unit_vector(5)),
            ),
        ],
    )


def _seed_project_items(cursor) -> None:
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
            field_values,
            html_url,
            source_updated_at,
            embedding,
            metadata
        ) VALUES (
            %s,
            'DripDropz',
            5,
            %s,
            'issue',
            NULL,
            NULL,
            %s,
            %s,
            'DripDropz/daedalus',
            'In Progress',
            'P1',
            'M',
            'feature',
            'agentic',
            'phase-5',
            'high',
            '{}'::jsonb,
            %s,
            '2026-03-29T00:00:00Z'::timestamptz,
            %s::vector,
            '{}'::jsonb
        )
        """,
        [
            (
                "project:search-quality",
                "PVTI_search_quality",
                "Add search quality fixtures",
                "Add deterministic hybrid search quality regression coverage for the Daedalus KB",
                "https://example.com/project/search-quality",
                _vector_literal(_unit_vector(0)),
            ),
            (
                "project:search-smoke",
                "PVTI_search_smoke",
                "Search smoke followup",
                "Preserve deterministic search smoke checks after fixture coverage lands",
                "https://example.com/project/search-smoke",
                _vector_literal(_unit_vector(6)),
            ),
        ],
    )


def _sanitized_sql(path: Path) -> str:
    lines: list[str] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip().startswith("\\ir "):
            continue
        lines.append(line)
    return "\n".join(lines)


def _unit_vector(index: int, *, dimensions: int = 384) -> list[float]:
    return [1.0 if position == index else 0.0 for position in range(dimensions)]


FIXTURE_QUERY_EMBEDDINGS: Mapping[str, tuple[float, ...]] = {
    "code_fixture_loader": tuple(_unit_vector(3)),
    "hybrid_search_quality": tuple(_unit_vector(0)),
}


def _vector_literal(values: Sequence[float]) -> str:
    return "[" + ",".join(format(float(value), ".17g") for value in values) + "]"
