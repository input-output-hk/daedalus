'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.isNonRecommendedDecimalSettingUsed = void 0;
const isNonRecommendedDecimalSettingUsed = ({
  recommendedDecimals,
  decimals,
}) => {
  const hasRecommendedDecimals = typeof recommendedDecimals === 'number';
  const hasConfiguredDecimals = typeof decimals === 'number';
  if (!hasRecommendedDecimals) {
    return false;
  }
  if (hasConfiguredDecimals) {
    return decimals !== recommendedDecimals;
  }
  if (!hasConfiguredDecimals && recommendedDecimals === 0) {
    return false;
  }
  return true;
};
exports.isNonRecommendedDecimalSettingUsed = isNonRecommendedDecimalSettingUsed;
//# sourceMappingURL=helpers.js.map
