// @flow
import { waitAndClick } from '../../../common/e2e/steps/helpers';
import type { Daedalus, WebdriverClient } from '../../../types';
import BigNumber from 'bignumber.js';

declare var daedalus: Daedalus;

const STAKING_BUTTON_SIDEBAR = '.SidebarCategory_component.staking';
const DELEGATION_CENTER_PAGE = '.StakingWithNavigation_page';

export const delegationCentreStakingHelper = {
  stakingButtonVisible: async (
    client: Object
  ) =>
    client.waitForVisible(STAKING_BUTTON_SIDEBAR),
  clickStakingButton: async (
    client: Object
  ) =>
    client.click(STAKING_BUTTON_SIDEBAR),
  delegationCenterVisible: async (
    client: Object
  ) =>
    client.waitForVisible(DELEGATION_CENTER_PAGE)
};

export const getStakePoolByRanking = async (client: Object, ranking: number) => {
  const result = await client.execute(ranking => {
    const stakePools = daedalus.stores.staking.stakePools;
    return stakePools.find(stakePool => stakePool.ranking === parseInt(ranking))
  }, ranking);
  return result.value;
};

export const getCardanoEpochData = async function(epoch: 'current' | 'next', param: string) {
  const headerIndex = epoch === 'current' ? 1 : 2;
  return await this.client.getText(`(//div[@class="DelegationCenterHeader_heading"])[${headerIndex}]//following-sibling::div//div[text()="${param}"]//following-sibling::div[@class="DelegationCenterHeader_fieldValue"]`);
};

export const getCurrentEpoch = async function() {
  return await getCardanoEpochData.call(this, 'current', 'Epoch');
};

export const getNextEpoch = async function() {
  const headerText = await this.client.getText(`(//div[@class="DelegationCenterHeader_heading"])[2]`);
  try {
    return headerText.match(/[1-9]+/)[0];
  } catch(err) {
    return new Error(err);
  }
};

export const baseWalletData = {
  id: 'modified-wallet-id',
  addressPoolGap: 0,
  name: 'Modified Wallet',
  amount: new BigNumber(50000),
  availableAmount: new BigNumber(50000),
  reward: new BigNumber(50000),
  passwordUpdateDate: new Date(),
  syncState: {
    status: {
      quantity: 10,
      unit: 'percentage',
    },
  },
  isLegacy: false,
  delegatedStakePoolId: null,
};
