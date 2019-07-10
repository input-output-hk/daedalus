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
  retiring?: Date,
  url: string,
};

export type Reward = {
  date: string,
  wallet: string,
  amount: number,
  pool: StakePool,
};
