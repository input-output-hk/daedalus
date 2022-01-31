type IsRecommendedDecimal = {
  decimals: number | null | undefined;
  recommendedDecimals: number | null | undefined;
};
export const isRecommendedDecimal = ({
  recommendedDecimals,
  decimals,
}: IsRecommendedDecimal) =>
  typeof recommendedDecimals === 'number' && decimals !== recommendedDecimals;
