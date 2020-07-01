// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type { DelegationAction } from '../api/staking/types';

export const DelegationActions: {
  CHANGE_DELEGATION: DelegationAction,
  REMOVE_DELEGATION: DelegationAction,
  DELEGATE: DelegationAction,
} = {
  CHANGE_DELEGATION: 'changeDelegation',
  REMOVE_DELEGATION: 'removeDelegation',
  DELEGATE: 'delegate',
};

export type StakePoolProps = {
  id: string,
  ticker: string,
  homepage: string,
  relativeStake: BigNumber,
  producedBlocks: number,
  description: string,
  cost: BigNumber,
  pledge: BigNumber,
  isCharity: boolean,
  name: string,
  profitMargin: number,
  ranking: number,
  retiring?: ?Date,
  saturation: number,
};

export default class StakePool {
  id: string;
  @observable ticker: string;
  @observable homepage: string;
  @observable producedBlocks: number;
  @observable relativeStake: BigNumber;
  @observable pledge: BigNumber;
  @observable cost: BigNumber;
  @observable description: string = '';
  @observable isCharity: boolean;
  @observable name: string = '';
  @observable profitMargin: number;
  @observable ranking: number;
  @observable retiring: ?Date;
  @observable saturation: number;

  constructor(data: StakePoolProps) {
    Object.assign(this, data);
  }
}
