// @flow

export type StakePoolProps = {
  ranking: number,
  id: string,
  name: string,
  description: string,
  url: string,
  controlledStake: number,
  profitMargin: number,
  performance: number,
  retirement?: Date,
};
