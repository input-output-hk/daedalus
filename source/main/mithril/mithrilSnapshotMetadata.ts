import type { MithrilSnapshotItem } from '../../common/types/mithril-bootstrap.types';

export type ResolvedLatestSnapshot = {
  snapshot: MithrilSnapshotItem;
  latestCertifiedImmutableNumber: number;
};

export const normalizeSnapshotItem = (
  raw: Record<string, any>
): MithrilSnapshotItem => {
  const digest = raw.digest || raw.snapshot_digest || raw.hash || '';
  const createdAt = raw.created_at || raw.createdAt || raw.timestamp || '';
  const size = Number(
    raw.size ??
      raw.total_size ??
      raw.total_db_size_uncompressed ??
      raw.size_bytes ??
      0
  );
  const cardanoNodeVersion =
    raw.cardano_node_version || raw.cardanoNodeVersion || raw.node_version;
  const network = raw.network || raw.cardano_network || raw.cardanoNetwork;
  return {
    digest,
    createdAt,
    size,
    cardanoNodeVersion,
    network,
  };
};

export const isNonEmptyString = (value: unknown): value is string =>
  typeof value === 'string' && value.trim().length > 0;

export const toTimestamp = (value: string): number => {
  const timestamp = Date.parse(value);
  return Number.isNaN(timestamp) ? 0 : timestamp;
};

const toPositiveInteger = (value: unknown): number | null => {
  if (typeof value === 'number' && Number.isInteger(value) && value >= 0) {
    return value;
  }

  if (typeof value === 'string' && /^\d+$/.test(value.trim())) {
    return Number(value);
  }

  return null;
};

const getNestedValue = (value: unknown, keyPath: Array<string>): unknown => {
  let current = value;

  for (const key of keyPath) {
    if (!current || typeof current !== 'object') {
      return undefined;
    }

    current = (current as Record<string, unknown>)[key];
  }

  return current;
};

export const extractLatestCertifiedImmutableNumber = (
  raw: Record<string, unknown>
): number | null => {
  const explicitPaths = [
    ['beacon', 'immutable_file_number'],
    ['beacon', 'immutableFileNumber'],
    ['cardano_db_beacon', 'immutable_file_number'],
    ['cardanoDbBeacon', 'immutableFileNumber'],
    ['immutable_file_number'],
    ['immutableFileNumber'],
    ['last_immutable_file_number'],
    ['lastImmutableFileNumber'],
  ];

  for (const keyPath of explicitPaths) {
    const parsedNumber = toPositiveInteger(getNestedValue(raw, keyPath));
    if (parsedNumber != null) {
      return parsedNumber;
    }
  }

  return null;
};

export const parseMithrilJson = (
  payload: string,
  onWarn: (details: { error: unknown; payload: string }) => void
): any => {
  try {
    return JSON.parse(payload);
  } catch (error) {
    const lines = payload
      .split('\n')
      .map((line) => line.trim())
      .filter(Boolean);
    for (let index = lines.length - 1; index >= 0; index -= 1) {
      const line = lines[index];
      if (line.startsWith('{') || line.startsWith('[')) {
        try {
          return JSON.parse(line);
        } catch (nestedError) {
          onWarn({
            error: nestedError,
            payload: line.slice(0, 200),
          });
          break;
        }
      }
    }

    onWarn({
      error,
      payload: payload?.slice(0, 200),
    });
    return null;
  }
};

export const normalizeResolvedLatestSnapshot = (
  raw: unknown
): ResolvedLatestSnapshot | null => {
  if (!raw || typeof raw !== 'object') {
    return null;
  }

  const normalizedSnapshot = normalizeSnapshotItem(
    raw as Record<string, unknown>
  );
  const latestCertifiedImmutableNumber = extractLatestCertifiedImmutableNumber(
    raw as Record<string, unknown>
  );

  if (latestCertifiedImmutableNumber == null) {
    return null;
  }

  return {
    snapshot: normalizedSnapshot,
    latestCertifiedImmutableNumber,
  };
};
