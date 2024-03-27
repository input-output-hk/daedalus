'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const helpers_1 = require('./helpers');
describe('isNonRecommendedDecimalSettingUsed', () => {
  it('returns false if asset does not have recommended decimals', async () => {
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 0,
        recommendedDecimals: undefined,
      })
    ).toEqual(false);
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 5,
        recommendedDecimals: undefined,
      })
    ).toEqual(false);
  });
  it('returns false if recommended decimal settings are applied by user', async () => {
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 0,
        recommendedDecimals: 0,
      })
    ).toEqual(false);
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 5,
        recommendedDecimals: 5,
      })
    ).toEqual(false);
  });
  it('returns false if 0 (default value) is recommended and user never changed settings', async () => {
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: undefined,
        recommendedDecimals: 0,
      })
    ).toEqual(false);
  });
  it('returns true if non-zero decimals are recommended but user never changed settings', async () => {
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: undefined,
        recommendedDecimals: 3,
      })
    ).toEqual(true);
  });
  it('returns true if user applied non-recommended decimal settings', async () => {
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 3,
        recommendedDecimals: 0,
      })
    ).toEqual(true);
    expect(
      (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
        decimals: 0,
        recommendedDecimals: 3,
      })
    ).toEqual(true);
  });
});
//# sourceMappingURL=helpers.spec.js.map
