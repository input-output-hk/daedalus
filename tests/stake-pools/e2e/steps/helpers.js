// @flow

import type { Daedalus } from '../../../types';

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