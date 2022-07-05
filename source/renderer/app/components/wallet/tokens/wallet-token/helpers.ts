type IsNonRecommendedDecimalSettingUsedArgs = {
  decimals: number | null | undefined;
  recommendedDecimals: number | null | undefined;
};

export const isNonRecommendedDecimalSettingUsed = ({
  recommendedDecimals,
  decimals,
}: IsNonRecommendedDecimalSettingUsedArgs) => {
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
