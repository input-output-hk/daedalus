// @flow

type IsRecommendedDecimal = {
  decimals: ?number,
  recommendedDecimals: ?number,
};

export const isRecommendedDecimal = ({
  recommendedDecimals,
  decimals,
}: IsRecommendedDecimal) =>
  typeof recommendedDecimals === 'number' && decimals !== recommendedDecimals;
