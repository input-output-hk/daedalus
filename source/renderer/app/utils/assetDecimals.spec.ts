import { AssetDecimalsProvenance, resolveAssetDecimals } from './assetDecimals';

describe('resolveAssetDecimals', () => {
  it('prefers an explicit user setting over a verified registry value', () => {
    expect(
      resolveAssetDecimals({
        userDecimals: 2,
        registryDecimals: 6,
        registryDecimalsVerified: true,
      })
    ).toEqual({
      decimals: 2,
      provenance: AssetDecimalsProvenance.UserSetting,
    });
  });

  it('keeps a user setting of zero against a verified non-zero value', () => {
    // Truthiness on the setting would drop this one and format the amount to
    // six places against the user's explicit instruction not to.
    expect(
      resolveAssetDecimals({
        userDecimals: 0,
        registryDecimals: 6,
        registryDecimalsVerified: true,
      })
    ).toEqual({
      decimals: 0,
      provenance: AssetDecimalsProvenance.UserSetting,
    });
  });

  it('applies a verified registry value when there is no user setting', () => {
    expect(
      resolveAssetDecimals({
        registryDecimals: 6,
        registryDecimalsVerified: true,
      })
    ).toEqual({
      decimals: 6,
      provenance: AssetDecimalsProvenance.VerifiedRegistry,
    });
  });

  it('applies a verified value of zero', () => {
    expect(
      resolveAssetDecimals({
        registryDecimals: 0,
        registryDecimalsVerified: true,
      })
    ).toEqual({
      decimals: 0,
      provenance: AssetDecimalsProvenance.VerifiedRegistry,
    });
  });

  it('never applies an unverified registry value', () => {
    expect(
      resolveAssetDecimals({
        registryDecimals: 6,
        registryDecimalsVerified: false,
      })
    ).toEqual({
      decimals: null,
      provenance: AssetDecimalsProvenance.None,
    });
  });

  it('never applies a registry value with no verdict at all', () => {
    expect(
      resolveAssetDecimals({
        registryDecimals: 6,
      })
    ).toEqual({
      decimals: null,
      provenance: AssetDecimalsProvenance.None,
    });
  });

  it('resolves to none when neither source has a value', () => {
    expect(resolveAssetDecimals({})).toEqual({
      decimals: null,
      provenance: AssetDecimalsProvenance.None,
    });
  });

  it('treats both spellings of an absent user setting alike', () => {
    const fromUndefined = resolveAssetDecimals({
      userDecimals: undefined,
      registryDecimals: 6,
      registryDecimalsVerified: true,
    });
    const fromNull = resolveAssetDecimals({
      userDecimals: null,
      registryDecimals: 6,
      registryDecimalsVerified: true,
    });
    expect(fromUndefined).toEqual(fromNull);
    expect(fromNull.decimals).toEqual(6);
  });

  it('resolves to none when the registry published nothing but verified', () => {
    // `verified` is the verdict for the decimals property, and a subject with no
    // decimals property carries `false`. This case is the belt to that brace:
    // even a true verdict formats nothing without a number to format with.
    expect(
      resolveAssetDecimals({
        registryDecimals: null,
        registryDecimalsVerified: true,
      })
    ).toEqual({
      decimals: null,
      provenance: AssetDecimalsProvenance.None,
    });
  });
});
