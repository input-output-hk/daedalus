// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';

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
  cost: BigNumber,
  pledge: BigNumber,
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
  @observable name: string = '';
  @observable profitMargin: number;
  @observable ranking: number;
  @observable retiring: ?Date;

  constructor(data: StakePoolProps) {
    // console.log('data', data);
    Object.assign(this, data);
  }
}
