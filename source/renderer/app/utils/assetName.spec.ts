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
