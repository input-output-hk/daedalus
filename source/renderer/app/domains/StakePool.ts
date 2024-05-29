import { observable, makeObservable } from 'mobx';
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
  ticker: string;
  homepage: string;
  producedBlocks: number;
  potentialRewards: BigNumber;
  nonMyopicMemberRewards: number;
  relativeStake: BigNumber;
  pledge: BigNumber;
  cost: BigNumber;
  description = '';
  isCharity: boolean;
  name = '';
  profitMargin: number;
  ranking: number;
  retiring: Date | null | undefined;
  saturation: number;

  constructor(data: StakePoolProps) {
    makeObservable(this, {
      ticker: observable,
      homepage: observable,
      producedBlocks: observable,
      potentialRewards: observable,
      nonMyopicMemberRewards: observable,
      relativeStake: observable,
      pledge: observable,
      cost: observable,
      description: observable,
      isCharity: observable,
      name: observable,
      profitMargin: observable,
      ranking: observable,
      retiring: observable,
      saturation: observable,
    });

    Object.assign(this, data);
  }
}
