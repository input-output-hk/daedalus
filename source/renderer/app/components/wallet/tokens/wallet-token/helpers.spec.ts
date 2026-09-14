import {
  DecimalSettingDisagreement,
  decimalSettingDisagreement,
} from './helpers';

describe('decimalSettingDisagreement', () => {
  it('returns none if asset does not have recommended decimals', async () => {
    expect(
      decimalSettingDisagreement({
        decimals: 0,
        recommendedDecimals: undefined,
      })
    ).toEqual(DecimalSettingDisagreement.None);

    expect(
      decimalSettingDisagreement({
        decimals: 5,
        recommendedDecimals: undefined,
      })
    ).toEqual(DecimalSettingDisagreement.None);
  });

  it('returns none if recommended decimal settings are applied by user', async () => {
    expect(
      decimalSettingDisagreement({
        decimals: 0,
        recommendedDecimals: 0,
      })
    ).toEqual(DecimalSettingDisagreement.None);

    expect(
      decimalSettingDisagreement({
        decimals: 5,
        recommendedDecimals: 5,
      })
    ).toEqual(DecimalSettingDisagreement.None);
  });

  it('returns none if 0 (default value) is recommended and user never changed settings', async () => {
    expect(
      decimalSettingDisagreement({
        decimals: undefined,
        recommendedDecimals: 0,
      })
    ).toEqual(DecimalSettingDisagreement.None);
  });

  it('reports a disagreement if non-zero decimals are recommended but user never changed settings', async () => {
    expect(
      decimalSettingDisagreement({
        decimals: undefined,
        recommendedDecimals: 3,
      })
    ).toEqual(DecimalSettingDisagreement.WithUnverified);
  });

  it('reports a disagreement if user applied non-recommended decimal settings', async () => {
    expect(
      decimalSettingDisagreement({
        decimals: 3,
        recommendedDecimals: 0,
      })
    ).toEqual(DecimalSettingDisagreement.WithUnverified);

    expect(
      decimalSettingDisagreement({
        decimals: 0,
        recommendedDecimals: 3,
      })
    ).toEqual(DecimalSettingDisagreement.WithUnverified);
  });

  describe('the verification verdict', () => {
    it('says nothing when the setting agrees with a verified value', () => {
      expect(
        decimalSettingDisagreement({
          decimals: 6,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: true,
        })
      ).toEqual(DecimalSettingDisagreement.None);
    });

    it('puts a disagreement with a verified value more strongly', () => {
      expect(
        decimalSettingDisagreement({
          decimals: 2,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: true,
        })
      ).toEqual(DecimalSettingDisagreement.WithVerified);
    });

    it('puts a disagreement with an unverified value more weakly', () => {
      expect(
        decimalSettingDisagreement({
          decimals: 2,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: false,
        })
      ).toEqual(DecimalSettingDisagreement.WithUnverified);
    });

    it('treats an absent verdict as unverified', () => {
      // The weaker claim is the safe one: an argument object that has not been
      // told the value was checked must not say it was.
      expect(
        decimalSettingDisagreement({
          decimals: 2,
          recommendedDecimals: 6,
        })
      ).toEqual(DecimalSettingDisagreement.WithUnverified);

      expect(
        decimalSettingDisagreement({
          decimals: 2,
          recommendedDecimals: 6,
          recommendedDecimalsVerified: null,
        })
      ).toEqual(DecimalSettingDisagreement.WithUnverified);
    });

    it('still says nothing when there is no published value to disagree with', () => {
      expect(
        decimalSettingDisagreement({
          decimals: 2,
          recommendedDecimals: null,
          recommendedDecimalsVerified: true,
        })
      ).toEqual(DecimalSettingDisagreement.None);
    });
  });
});
