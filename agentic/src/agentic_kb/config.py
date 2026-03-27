from __future__ import annotations

import os
from dataclasses import dataclass
from typing import List
from urllib.parse import urlparse


DEFAULT_OLLAMA_BASE_URL = "http://ollama:11434"
DEFAULT_OLLAMA_EMBED_MODEL = "all-minilm"


@dataclass(frozen=True)
class DatabaseEndpoint:
    scheme: str
    host: str
    port: int
    database: str


@dataclass(frozen=True)
class AgenticConfig:
    database_url: str | None
    ollama_base_url: str
    ollama_embed_model: str
    github_token: str | None

    @classmethod
    def from_env(cls) -> "AgenticConfig":
        database_url = _clean_env(os.getenv("DATABASE_URL"))
        ollama_base_url = _clean_env(os.getenv("OLLAMA_BASE_URL")) or DEFAULT_OLLAMA_BASE_URL
        ollama_embed_model = _clean_env(os.getenv("OLLAMA_EMBED_MODEL")) or DEFAULT_OLLAMA_EMBED_MODEL
        github_token = _clean_env(os.getenv("GITHUB_TOKEN"))

        return cls(
            database_url=database_url,
            ollama_base_url=ollama_base_url,
            ollama_embed_model=ollama_embed_model,
            github_token=github_token,
        )

    def runtime_errors(self) -> List[str]:
        errors: List[str] = []

        if not self.database_url:
            errors.append("DATABASE_URL is required for service mode and healthchecks")
        else:
            try:
                parse_database_endpoint(self.database_url)
            except ValueError as error:
                errors.append(str(error))

        if not self.ollama_base_url:
            errors.append("OLLAMA_BASE_URL is required for service mode and healthchecks")

        if not self.ollama_embed_model:
            errors.append("OLLAMA_EMBED_MODEL is required for service mode and healthchecks")

        return errors


def parse_database_endpoint(database_url: str) -> DatabaseEndpoint:
    parsed = urlparse(database_url)

    if parsed.scheme not in {"postgres", "postgresql"}:
        raise ValueError(
            "DATABASE_URL must use postgres or postgresql scheme for task-103 bootstrap checks"
        )

    if not parsed.hostname:
        raise ValueError("DATABASE_URL must include a host for task-103 bootstrap checks")

    database = parsed.path.lstrip("/")
    if not database:
        raise ValueError("DATABASE_URL must include a database name for task-103 bootstrap checks")

    return DatabaseEndpoint(
        scheme=parsed.scheme,
        host=parsed.hostname,
        port=parsed.port or 5432,
        database=database,
    )


def _clean_env(value: str | None) -> str | None:
    if value is None:
        return None

    stripped = value.strip()
    return stripped or None
