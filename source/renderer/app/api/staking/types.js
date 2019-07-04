// @flow

export type StakePool = {
  id: string,
  controlledStake: number,
  description: string,
  slug: string,
  name: string,
  performance: number,
  profitMargin: number,
  ranking: number,
  retirement?: Date,
  url: string,
};
