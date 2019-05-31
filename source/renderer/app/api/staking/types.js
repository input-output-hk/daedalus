// @flow

export type StakePoolProps = {
  index: number,
  id: string,
  name: string,
  description: string,
  url: string,
  controlledStake: number,
  profitMargin: number,
  performance: number,
  retirement?: Date,
};
