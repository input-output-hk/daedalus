from __future__ import annotations

from datetime import date, datetime, timezone
from decimal import Decimal
from enum import Enum
import json
from pathlib import Path
import sys
from typing import Any, Mapping, Sequence
from uuid import UUID


def json_safe_value(value: Any) -> Any:
    if isinstance(value, Enum):
        return value.value
    if isinstance(value, datetime):
        return value.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")
    if isinstance(value, date):
        return value.isoformat()
    if isinstance(value, Decimal):
        return float(value)
    if isinstance(value, (Path, UUID)):
        return str(value)
    if isinstance(value, Mapping):
        return {str(key): json_safe_value(item) for key, item in value.items()}
    if isinstance(value, Sequence) and not isinstance(value, (str, bytes, bytearray)):
        return [json_safe_value(item) for item in value]
    return value


def print_json(payload: Mapping[str, Any]) -> None:
    print(json.dumps(json_safe_value(payload), ensure_ascii=True, separators=(",", ":")))


def print_stderr(message: str) -> None:
    print(message, file=sys.stderr)


def format_text_value(value: Any) -> str:
    normalized = json_safe_value(value)
    if isinstance(normalized, str):
        return normalized
    return json.dumps(normalized, ensure_ascii=True, separators=(",", ":"))
