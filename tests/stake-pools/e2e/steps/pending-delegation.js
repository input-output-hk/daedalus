// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';
import { getWalletByName } from '../../../wallets/e2e/steps/helpers';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';

declare var daedalus: Daedalus;

type Status = 'delegating' | 'not_delegating' | 'none';
type EpochType = 'current' | 'next' | 'last';
type DelegationData = {
    status: Status,
    target?: string,
    changes_at?: Object,
};

Given(/^the wallet has (.*), (.*) and (.*) delegation data$/, async function(currentDelegation, nextDelegation, lastDelegation) {
  await this.client.waitUntil(async () => {
    const stakePools = await this.client.execute(() => daedalus.stores.staking.stakePools);
    return stakePools.value.length;
  });
  await this.client.executeAsync((currentDelegation, nextDelegation, lastDelegation, done) => {
    const getData = (epoch: EpochType, status: Status, target: string): DelegationData => {
      let delegation: DelegationData = {
        status,
      };
      if (status === 'delegating') {
        delegation.target = target;
      }
      if (status !== 'none') {
        if (epoch !== 'next') {
          nextDelegationStakePoolStatus = status;
        }
        if (epoch !== 'last') {
          lastDelegationStakePoolStatus = status;
        }
      }
      if (epoch !== 'current') {
        delegation.changes_at = {
          epoch_start_time: '2020-02-02T02:02:57Z',
          epoch_number: 123456789,
        };
      }
      return delegation;
    }
    const { stakePools } = daedalus.stores.staking;
    const currentDelegationData = getData('current', currentDelegation, stakePools[0].id);
    const delegatedStakePoolId = currentDelegationData.target;
    const nextDelegationStakePoolEpoch = 123456789;
    let nextDelegationStakePoolId = null;
    let nextDelegationStakePoolStatus = null;
    const lastDelegationStakePoolEpoch = 123456789;
    let lastDelegationStakePoolId = null;
    let lastDelegationStakePoolStatus = null;
    let nextData = [];
    let nextDelegationData = null;
    if (nextDelegation !== 'none') {
      nextDelegationData = getData('next', nextDelegation, stakePools[1].id);
      nextData.push(nextDelegationData);
      nextDelegationStakePoolId = nextDelegationData.target;
      nextDelegationStakePoolStatus = nextDelegationData.status;
    }
    if (lastDelegation !== 'none') {
      const lastDelegationData = getData('last', lastDelegation, stakePools[2].id);
      nextData.push(lastDelegationData);
      lastDelegationStakePoolId = lastDelegationData.target;
      lastDelegationStakePoolStatus = lastDelegationData.status;
    }
    const modifiedWallet = {
      delegatedStakePoolId,
      nextDelegationStakePoolEpoch,
      nextDelegationStakePoolId,
      nextDelegationStakePoolStatus,
      lastDelegationStakePoolEpoch,
      lastDelegationStakePoolId,
      lastDelegationStakePoolStatus,
      pendingDelegations: nextDelegationData,
    };
    daedalus.api.ada.setWalletForPendingDelegation(modifiedWallet).then(done);
  }, currentDelegation, nextDelegation, lastDelegation);
});

Then(/^the Pending Delegation information should be (.*)$/, async function(showPendingMenu) {
  console.log('showPendingMenu', showPendingMenu);
});

Then(/^the wallet should be displayed as (.*)$/, async function(displayAsDelegated) {
  console.log('displayAsDelegated', displayAsDelegated);
});



