import {
  extractLatestCertifiedImmutableNumber,
  normalizeResolvedLatestSnapshot,
  normalizeSnapshotItem,
  parseMithrilJson,
  toTimestamp,
} from './mithrilSnapshotMetadata';

describe('mithrilSnapshotMetadata', () => {
  it('normalizes snapshot fields across Mithril JSON variants', () => {
    expect(
      normalizeSnapshotItem({
        hash: 'digest-1',
        timestamp: '2026-05-20T00:00:00Z',
        total_db_size_uncompressed: '42',
        node_version: '10.4.0',
        cardano_network: 'mainnet',
      })
    ).toEqual({
      digest: 'digest-1',
      createdAt: '2026-05-20T00:00:00Z',
      size: 42,
      cardanoNodeVersion: '10.4.0',
      network: 'mainnet',
    });
  });

  it('extracts the latest certified immutable number from known beacon locations', () => {
    expect(
      extractLatestCertifiedImmutableNumber({
        cardano_db_beacon: { immutable_file_number: '25' },
      })
    ).toBe(25);
  });

  it('returns null when immutable metadata is missing', () => {
    expect(normalizeResolvedLatestSnapshot({ digest: 'missing-number' })).toBeNull();
  });

  it('parses trailing JSON lines from Mithril command output', () => {
    const warn = jest.fn();

    expect(
      parseMithrilJson('noise\n{"digest":"digest-1"}', warn)
    ).toEqual({ digest: 'digest-1' });
    expect(warn).not.toHaveBeenCalled();
  });

  it('keeps invalid timestamps sortable without throwing', () => {
    expect(toTimestamp('invalid-date')).toBe(0);
  });
});
