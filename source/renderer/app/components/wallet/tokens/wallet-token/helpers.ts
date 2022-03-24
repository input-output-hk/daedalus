type IsRecommendedDecimal = {
  decimals: number | null | undefined;
  recommendedDecimals: number | null | undefined;
};

export const isRecommendedDecimal = ({
  recommendedDecimals,
  decimals,
}: IsRecommendedDecimal) => {
  return (
    typeof recommendedDecimals === 'number' &&
    typeof decimals === 'number' &&
    decimals !== recommendedDecimals
  );
};
