from __future__ import annotations

import io
import json
import unittest
from urllib.error import HTTPError, URLError
from unittest.mock import patch

from agentic_kb.config import AgenticConfig
from agentic_kb.embed import (
    EXPECTED_EMBEDDING_DIMENSION,
    EmbeddingConnectionError,
    EmbeddingDimensionError,
    EmbeddingModelNotFoundError,
    EmbeddingResponseError,
    EmbeddingValidationError,
    OllamaEmbeddingClient,
)


class FakeResponse:
    def __init__(self, payload, *, encoding: str = "utf-8"):
        self._payload = payload
        self._encoding = encoding

    def read(self) -> bytes:
        if isinstance(self._payload, bytes):
            return self._payload
        if isinstance(self._payload, str):
            return self._payload.encode(self._encoding)
        return json.dumps(self._payload).encode(self._encoding)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


class OllamaEmbeddingClientTests(unittest.TestCase):
    def setUp(self) -> None:
        self.config = AgenticConfig(
            database_url=None,
            ollama_base_url="http://ollama:11434",
            ollama_embed_model="all-minilm",
            github_token=None,
        )
        self.client = OllamaEmbeddingClient.from_config(self.config)

    def test_embed_text_returns_vector_from_embeddings_payload(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse({"embeddings": [_vector()]})) as mocked_urlopen:
            vector = self.client.embed_text("mithril bootstrap")

        self.assertEqual(len(vector), EXPECTED_EMBEDDING_DIMENSION)
        self.assertTrue(all(isinstance(value, float) for value in vector))

        request = mocked_urlopen.call_args.args[0]
        self.assertEqual(request.full_url, "http://ollama:11434/api/embed")
        self.assertEqual(request.get_method(), "POST")
        self.assertEqual(request.headers["Content-type"], "application/json")

        body = json.loads(request.data.decode("utf-8"))
        self.assertEqual(body, {"model": "all-minilm", "input": "mithril bootstrap"})

    def test_embed_texts_returns_vectors_in_input_order(self):
        with patch(
            "agentic_kb.embed.client.urlopen",
            return_value=FakeResponse({"embeddings": [_vector(fill=1.0), _vector(fill=2.0)]}),
        ):
            vectors = self.client.embed_texts(["alpha", "beta"])

        self.assertEqual(len(vectors), 2)
        self.assertEqual(vectors[0][0], 1.0)
        self.assertEqual(vectors[1][0], 2.0)

    def test_embed_text_supports_single_embedding_payload_shape(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse({"embedding": _vector(fill=3.0)})):
            vector = self.client.embed_text("mithril bootstrap")

        self.assertEqual(vector[0], 3.0)

    def test_embed_text_rejects_empty_input_before_http_call(self):
        with patch("agentic_kb.embed.client.urlopen") as mocked_urlopen:
            with self.assertRaisesRegex(EmbeddingValidationError, "text must not be empty"):
                self.client.embed_text("   ")

        mocked_urlopen.assert_not_called()

    def test_embed_texts_returns_empty_list_without_http_call_for_empty_batch(self):
        with patch("agentic_kb.embed.client.urlopen") as mocked_urlopen:
            vectors = self.client.embed_texts([])

        self.assertEqual(vectors, [])
        mocked_urlopen.assert_not_called()

    def test_embed_texts_rejects_whitespace_item_with_index(self):
        with patch("agentic_kb.embed.client.urlopen") as mocked_urlopen:
            with self.assertRaisesRegex(EmbeddingValidationError, r"texts\[1\] must not be empty"):
                self.client.embed_texts(["alpha", "   "])

        mocked_urlopen.assert_not_called()

    def test_embed_text_raises_connection_error_when_ollama_is_unreachable(self):
        with patch("agentic_kb.embed.client.urlopen", side_effect=URLError("connection refused")):
            with self.assertRaisesRegex(EmbeddingConnectionError, "Unable to reach Ollama embed API"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_response_error_for_invalid_json(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse("not json")):
            with self.assertRaisesRegex(EmbeddingResponseError, "invalid JSON"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_response_error_for_missing_embedding_payload(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse({"model": "all-minilm"})):
            with self.assertRaisesRegex(EmbeddingResponseError, "missing 'embedding' or 'embeddings'"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_response_error_for_non_list_embeddings_field(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse({"embeddings": "not-a-list"})):
            with self.assertRaisesRegex(EmbeddingResponseError, "field 'embeddings' must be a list"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_response_error_for_non_numeric_embedding_content(self):
        with patch(
            "agentic_kb.embed.client.urlopen",
            return_value=FakeResponse({"embeddings": [[1.0, "oops"]]}),
        ):
            with self.assertRaisesRegex(EmbeddingResponseError, "non-numeric value"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_dimension_error_for_unexpected_vector_size(self):
        with patch("agentic_kb.embed.client.urlopen", return_value=FakeResponse({"embeddings": [[1.0, 2.0]]})):
            with self.assertRaisesRegex(EmbeddingDimensionError, "expected 384"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_model_not_found_for_missing_configured_model(self):
        error = HTTPError(
            url="http://ollama:11434/api/embed",
            code=404,
            msg="Not Found",
            hdrs=None,
            fp=io.BytesIO(b'{"error":"model \'missing\' not found"}'),
        )
        with patch("agentic_kb.embed.client.urlopen", side_effect=error):
            with self.assertRaisesRegex(EmbeddingModelNotFoundError, "missing"):
                OllamaEmbeddingClient(base_url="http://ollama:11434", model="missing").embed_text(
                    "mithril bootstrap"
                )

    def test_embed_text_does_not_misclassify_generic_404_as_missing_model(self):
        error = HTTPError(
            url="http://ollama:11434/api/embed",
            code=404,
            msg="Not Found",
            hdrs=None,
            fp=io.BytesIO(b'{"error":"endpoint not found"}'),
        )
        with patch("agentic_kb.embed.client.urlopen", side_effect=error):
            with self.assertRaisesRegex(EmbeddingResponseError, "HTTP 404"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_does_not_misclassify_other_model_not_found_detail(self):
        error = HTTPError(
            url="http://ollama:11434/api/embed",
            code=404,
            msg="Not Found",
            hdrs=None,
            fp=io.BytesIO(b'{"error":"model \'different\' not found"}'),
        )
        with patch("agentic_kb.embed.client.urlopen", side_effect=error):
            with self.assertRaisesRegex(EmbeddingResponseError, "HTTP 404"):
                self.client.embed_text("mithril bootstrap")

    def test_embed_text_raises_response_error_for_generic_http_failure(self):
        error = HTTPError(
            url="http://ollama:11434/api/embed",
            code=500,
            msg="Internal Server Error",
            hdrs=None,
            fp=io.BytesIO(b'{"error":"backend exploded"}'),
        )
        with patch("agentic_kb.embed.client.urlopen", side_effect=error):
            with self.assertRaisesRegex(EmbeddingResponseError, "HTTP 500"):
                self.client.embed_text("mithril bootstrap")


def _vector(*, fill: float = 0.0) -> list[float]:
    return [float(fill)] * EXPECTED_EMBEDDING_DIMENSION


if __name__ == "__main__":
    unittest.main()
