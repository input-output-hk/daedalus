// @flow

export type StakePool = {
  controlledStake: number,
  description: string,
  id: string,
  name: string,
  performance: number,
  profitMargin: number,
  ranking: number,
  retirement?: Date,
  url: string,
};
