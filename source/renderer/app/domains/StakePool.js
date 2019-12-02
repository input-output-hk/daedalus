// @flow
import { observable } from 'mobx';

export type StakePoolProps = {
  id: string,
  ticker: string,
  homepage: string,
  pledgeAddress: string,
  performance: number,
  controlledStake: number,
  producedBlocks: number,

  createdAt: Date,
  description: string,
  isCharity: boolean,
  name: string,
  profitMargin: number,
  ranking: number,
  ranking: number,
  retiring?: Date,
};

export default class StakePool {
  id: string;
  @observable ticker: string;
  @observable homepage: string;
  @observable pledgeAddress: string;
  @observable performance: number;
  @observable producedBlocks: number;
  @observable controlledStake: number;

  @observable createdAt: Date;
  @observable description: string;
  @observable isCharity: boolean;
  @observable name: string;
  @observable profitMargin: number;
  @observable ranking: number;
  @observable ranking: number;
  @observable retiring: ?Date;

  constructor(data: StakePoolProps) {
    Object.assign(this, data);
  }
}
