from __future__ import annotations

import json
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import urljoin
from urllib.request import Request, urlopen

from agentic_kb.config import AgenticConfig


EXPECTED_EMBEDDING_DIMENSION = 384
DEFAULT_REQUEST_TIMEOUT_SECONDS = 10
EMBED_PATH = "api/embed"


class EmbeddingClientError(RuntimeError):
    pass


class EmbeddingValidationError(EmbeddingClientError):
    pass


class EmbeddingConnectionError(EmbeddingClientError):
    pass


class EmbeddingResponseError(EmbeddingClientError):
    pass


class EmbeddingModelNotFoundError(EmbeddingResponseError):
    pass


class EmbeddingDimensionError(EmbeddingClientError):
    pass


class OllamaEmbeddingClient:
    def __init__(
        self,
        *,
        base_url: str,
        model: str,
        expected_dimension: int = EXPECTED_EMBEDDING_DIMENSION,
        timeout_seconds: int = DEFAULT_REQUEST_TIMEOUT_SECONDS,
    ):
        self.base_url = base_url.rstrip("/")
        self.model = model
        self.expected_dimension = expected_dimension
        self.timeout_seconds = timeout_seconds

    @classmethod
    def from_config(cls, config: AgenticConfig) -> "OllamaEmbeddingClient":
        return cls(base_url=config.ollama_base_url, model=config.ollama_embed_model)

    def embed_text(self, text: str) -> list[float]:
        self._validate_text(text)
        embeddings = self._request_embeddings(text, expected_count=1)
        return embeddings[0]

    def embed_texts(self, texts: list[str]) -> list[list[float]]:
        if not texts:
            return []

        for index, text in enumerate(texts):
            self._validate_text(text, index=index)

        return self._request_embeddings(texts, expected_count=len(texts))

    def _request_embeddings(self, input_value: str | list[str], *, expected_count: int) -> list[list[float]]:
        endpoint = self._embed_endpoint()
        payload = json.dumps({"model": self.model, "input": input_value}).encode("utf-8")
        request = Request(
            endpoint,
            data=payload,
            headers={"Content-Type": "application/json"},
            method="POST",
        )

        try:
            with urlopen(request, timeout=self.timeout_seconds) as response:
                body = response.read().decode("utf-8")
        except HTTPError as error:
            detail = _read_http_error_detail(error)

            if _is_model_not_found_error(detail=detail, model=self.model):
                raise EmbeddingModelNotFoundError(
                    f"Ollama model {self.model!r} is unavailable via {endpoint}: {detail}"
                ) from error

            raise EmbeddingResponseError(
                f"Ollama embed request failed with HTTP {error.code} via {endpoint}: {detail}"
            ) from error
        except (TimeoutError, URLError, OSError) as error:
            raise EmbeddingConnectionError(
                f"Unable to reach Ollama embed API at {endpoint}: {error}"
            ) from error

        try:
            response_payload = json.loads(body)
        except json.JSONDecodeError as error:
            raise EmbeddingResponseError(
                f"Ollama embed API at {endpoint} returned invalid JSON: {error}"
            ) from error

        embeddings = self._normalize_embeddings(response_payload)
        if len(embeddings) != expected_count:
            raise EmbeddingResponseError(
                f"Ollama embed API at {endpoint} returned {len(embeddings)} embeddings for {expected_count} inputs"
            )

        for index, vector in enumerate(embeddings):
            self._validate_dimension(vector, index=index)

        return embeddings

    def _embed_endpoint(self) -> str:
        return urljoin(self.base_url + "/", EMBED_PATH)

    def _normalize_embeddings(self, payload: Any) -> list[list[float]]:
        if not isinstance(payload, dict):
            raise EmbeddingResponseError("Ollama embed response must be a JSON object")

        if "embeddings" in payload:
            raw_embeddings = payload["embeddings"]
            if not isinstance(raw_embeddings, list):
                raise EmbeddingResponseError("Ollama embed response field 'embeddings' must be a list")

            if raw_embeddings and all(isinstance(value, (int, float)) for value in raw_embeddings):
                return [_coerce_vector(raw_embeddings, field_name="embeddings")]

            return [
                _coerce_vector(raw_vector, field_name=f"embeddings[{index}]")
                for index, raw_vector in enumerate(raw_embeddings)
            ]

        if "embedding" in payload:
            return [_coerce_vector(payload["embedding"], field_name="embedding")]

        raise EmbeddingResponseError("Ollama embed response is missing 'embedding' or 'embeddings'")

    def _validate_dimension(self, vector: list[float], *, index: int) -> None:
        if len(vector) != self.expected_dimension:
            raise EmbeddingDimensionError(
                f"Ollama model {self.model!r} returned embedding {index} with dimension {len(vector)}; "
                f"expected {self.expected_dimension} for VECTOR({self.expected_dimension})"
            )

    def _validate_text(self, text: str, *, index: int | None = None) -> None:
        label = "text" if index is None else f"texts[{index}]"

        if not isinstance(text, str):
            raise EmbeddingValidationError(f"{label} must be a string")

        if not text.strip():
            raise EmbeddingValidationError(
                f"{label} must not be empty or whitespace-only"
            )


def _coerce_vector(raw_vector: Any, *, field_name: str) -> list[float]:
    if not isinstance(raw_vector, list):
        raise EmbeddingResponseError(
            f"Ollama embed response field {field_name!r} must be a list of numbers"
        )

    vector: list[float] = []
    for index, value in enumerate(raw_vector):
        if not isinstance(value, (int, float)):
            raise EmbeddingResponseError(
                f"Ollama embed response field {field_name!r} contains a non-numeric value at index {index}"
            )
        vector.append(float(value))

    return vector


def _read_http_error_detail(error: HTTPError) -> str:
    try:
        raw_detail = error.read().decode("utf-8")
    except Exception:
        raw_detail = ""

    if raw_detail:
        try:
            payload = json.loads(raw_detail)
        except json.JSONDecodeError:
            return raw_detail

        if isinstance(payload, dict) and isinstance(payload.get("error"), str):
            return payload["error"]

        return raw_detail

    return error.reason or "request failed"


def _is_model_not_found_error(*, detail: str, model: str) -> bool:
    lowered_detail = detail.lower()
    lowered_model = model.lower()

    if "model" not in lowered_detail or "not found" not in lowered_detail:
        return False

    candidates = {lowered_model}
    if ":" not in lowered_model:
        candidates.add(f"{lowered_model}:latest")

    return any(candidate in lowered_detail for candidate in candidates)
