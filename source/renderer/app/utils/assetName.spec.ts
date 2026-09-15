import {
  AssetNameProvenance,
  isMinterChosenAssetName,
  resolveAssetName,
} from './assetName';

// 'Cointest' and 'USDC' respectively.
const printableAssetName = '436f696e74657374';
const impersonatingAssetName = '55534443';
const nonPrintableAssetName =
  '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf';

describe('resolveAssetName', () => {
  it('prefers the registry ticker over every other source', () => {
    expect(
      resolveAssetName({
        assetName: printableAssetName,
        metadata: {
          name: 'Test Coin',
          description: 'A test coin',
          ticker: 'TEST',
        },
      })
    ).toEqual({
      name: 'TEST',
      provenance: AssetNameProvenance.RegistryTicker,
    });
  });

  it('falls back to the registry name when no ticker is published', () => {
    expect(
      resolveAssetName({
        assetName: printableAssetName,
        metadata: {
          name: 'Test Coin',
          description: 'A test coin',
        },
      })
    ).toEqual({
      name: 'Test Coin',
      provenance: AssetNameProvenance.RegistryName,
    });
  });

  it('falls back to the decoded asset name when no metadata is published', () => {
    expect(
      resolveAssetName({
        assetName: printableAssetName,
      })
    ).toEqual({
      name: 'Cointest',
      provenance: AssetNameProvenance.MinterChosen,
    });
  });

  it('resolves nothing when the asset name is not printable', () => {
    expect(
      resolveAssetName({
        assetName: nonPrintableAssetName,
      })
    ).toBeNull();
  });

  it('resolves nothing for an asset carrying neither metadata nor a name', () => {
    expect(resolveAssetName({})).toBeNull();
  });

  it('marks a decoded name that spells a registry ticker as minter-chosen', () => {
    const impersonating = resolveAssetName({
      assetName: impersonatingAssetName,
    });
    const published = resolveAssetName({
      assetName: '',
      metadata: {
        name: 'USD Coin',
        description: 'A stablecoin',
        ticker: 'USDC',
      },
    });
    expect(impersonating.name).toBe(published.name);
    expect(isMinterChosenAssetName(impersonating)).toBe(true);
    expect(isMinterChosenAssetName(published)).toBe(false);
  });
});

describe('isMinterChosenAssetName', () => {
  it('returns false when no name resolved', () => {
    expect(isMinterChosenAssetName(null)).toBe(false);
  });
});

describe('resolveAssetName for a chain row', () => {
  // The whole reason a chain row's `source` reaches the renderer. Both a
  // registry name and a CIP-25 name arrive as `metadata.name`, because the
  // cache stores one name column, and the two are not the same claim.
  it('names a CIP-25 record as coming from the chain rather than the registry', () => {
    const resolved = resolveAssetName({
      assetName:
        '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf',
      metadata: { name: 'Northwind Demo', description: '' },
      source: 'chain',
    });
    expect(resolved).toEqual({
      name: 'Northwind Demo',
      provenance: AssetNameProvenance.ChainName,
    });
  });

  it('names the same value from the registry as a registry name', () => {
    const resolved = resolveAssetName({
      assetName: '',
      metadata: { name: 'Northwind Demo', description: '' },
      source: 'registry',
    });
    expect(resolved.provenance).toBe(AssetNameProvenance.RegistryName);
  });

  // A chain name is in the transaction that minted the asset, which had to
  // satisfy the minting policy, so it is not the unbound case the marker exists
  // for.
  it('does not mark a chain name as minter-chosen', () => {
    expect(
      isMinterChosenAssetName(
        resolveAssetName({
          assetName: '436f696e74657374',
          metadata: { name: 'Northwind Demo', description: '' },
          source: 'chain',
        })
      )
    ).toBe(false);
  });

  it('prefers a decoded name over nothing when a chain row carries no name', () => {
    const resolved = resolveAssetName({
      assetName: '436f696e74657374',
      metadata: { name: '', description: '' },
      source: 'chain',
    });
    expect(resolved).toEqual({
      name: 'Cointest',
      provenance: AssetNameProvenance.MinterChosen,
    });
  });
});
