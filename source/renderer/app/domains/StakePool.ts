import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type { DelegationAction } from '../api/staking/types';

export const DelegationActions: {
  CHANGE_DELEGATION: DelegationAction;
  REMOVE_DELEGATION: DelegationAction;
  DELEGATE: DelegationAction;
} = {
  CHANGE_DELEGATION: 'changeDelegation',
  REMOVE_DELEGATION: 'removeDelegation',
  DELEGATE: 'delegate',
};
export type StakePoolProps = {
  id: string;
  ticker: string;
  homepage: string;
  relativeStake: BigNumber;
  producedBlocks: number;
  potentialRewards: BigNumber;
  nonMyopicMemberRewards: number;
  description: string;
  cost: BigNumber;
  pledge: BigNumber;
  isCharity: boolean;
  name: string;
  profitMargin: number;
  ranking: number;
  retiring?: Date | null | undefined;
  saturation: number;
};
export default class StakePool {
  id: string;
  @observable
  ticker: string;
  @observable
  homepage: string;
  @observable
  producedBlocks: number;
  @observable
  potentialRewards: BigNumber;
  @observable
  nonMyopicMemberRewards: number;
  @observable
  relativeStake: BigNumber;
  @observable
  pledge: BigNumber;
  @observable
  cost: BigNumber;
  @observable
  description = '';
  @observable
  isCharity: boolean;
  @observable
  name = '';
  @observable
  profitMargin: number;
  @observable
  ranking: number;
  @observable
  retiring: Date | null | undefined;
  @observable
  saturation: number;

  constructor(data: StakePoolProps) {
    Object.assign(this, data);
  }
}
