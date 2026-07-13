import {
  extractCertifiedEpoch,
  extractLatestCertifiedImmutableNumber,
  hasKnownSnapshotSize,
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

  it('accepts only a positive finite snapshot size as known', () => {
    const item = (size: number) => ({
      digest: 'digest-1',
      createdAt: '2026-05-20T00:00:00Z',
      size,
    });

    expect(hasKnownSnapshotSize(item(42))).toBe(true);
    expect(hasKnownSnapshotSize(item(0))).toBe(false);
    expect(hasKnownSnapshotSize(item(-1))).toBe(false);
    expect(hasKnownSnapshotSize(item(Number('garbage')))).toBe(false);
    expect(hasKnownSnapshotSize(null)).toBe(false);
  });

  it('extracts the latest certified immutable number from known beacon locations', () => {
    expect(
      extractLatestCertifiedImmutableNumber({
        cardano_db_beacon: { immutable_file_number: '25' },
      })
    ).toBe(25);
  });

  it('returns null when immutable metadata is missing', () => {
    expect(
      normalizeResolvedLatestSnapshot({ digest: 'missing-number' })
    ).toBeNull();
  });

  it('extracts the certified beacon epoch from the production beacon shape', () => {
    // Mirror the real immutable fixture (cardano_db_beacon, string-valued) with an epoch added.
    expect(
      extractCertifiedEpoch({
        cardano_db_beacon: { epoch: 320, immutable_file_number: '25' },
      })
    ).toBe(320);
  });

  it('parses a string-valued certified epoch via toPositiveInteger', () => {
    expect(extractCertifiedEpoch({ beacon: { epoch: '512' } })).toBe(512);
  });

  it('ignores the bare top-level epoch key and non-beacon epoch spellings', () => {
    expect(extractCertifiedEpoch({ epoch: 7 })).toBeNull();
    expect(extractCertifiedEpoch({ beacon: { epoch_number: 9 } })).toBeNull();
    expect(extractCertifiedEpoch({ epoch: 7, beacon: { epoch: 320 } })).toBe(
      320
    );
  });

  it('returns null when no epoch key is present, while the immutable number still resolves (safe-degrade)', () => {
    const beaconWithoutEpoch = {
      cardano_db_beacon: { immutable_file_number: '25' },
    };

    expect(extractCertifiedEpoch(beaconWithoutEpoch)).toBeNull();
    expect(extractLatestCertifiedImmutableNumber(beaconWithoutEpoch)).toBe(25);
  });

  it('carries the certified epoch onto the resolved latest snapshot', () => {
    expect(
      normalizeResolvedLatestSnapshot({
        digest: 'latest-digest',
        cardano_db_beacon: { epoch: 320, immutable_file_number: '25' },
      })
    ).toEqual({
      snapshot: expect.objectContaining({ digest: 'latest-digest' }),
      latestCertifiedImmutableNumber: 25,
      certifiedEpoch: 320,
    });
  });

  it('parses trailing JSON lines from Mithril command output', () => {
    const warn = jest.fn();

    expect(parseMithrilJson('noise\n{"digest":"digest-1"}', warn)).toEqual({
      digest: 'digest-1',
    });
    expect(warn).not.toHaveBeenCalled();
  });

  it('keeps invalid timestamps sortable without throwing', () => {
    expect(toTimestamp('invalid-date')).toBe(0);
  });
});
