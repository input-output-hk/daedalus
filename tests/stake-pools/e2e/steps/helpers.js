// @flow

const STAKING_BUTTON_SIDEBAR = '.SidebarCategory_component.staking';
const DELEGATION_CENTER_PAGE = '.StakingWithNavigation_page';
const STAKE_POOL_TAB_BUTTON = '.stake-pools.NavButton_component.NavButton_normal';
const STAKE_POOL_PAGE = '.StakePools_component';

export const delegationCentreStakingHelper = {
  stakingButtonVisible: async (
    client: Object
  ) =>
    client.waitForVisible(STAKING_BUTTON_SIDEBAR),
  clickStakingButton: async (
    client: Object,
  ) =>
    client.click(STAKING_BUTTON_SIDEBAR),
  delegationCenterVisible: async (
    client: Object,
  ) =>
    client.waitForVisible(DELEGATION_CENTER_PAGE),
  clickStakePoolsTab: async (
    client: Object,
  ) =>
    client.click(STAKE_POOL_TAB_BUTTON),
  stakingPoolVisible: async (
    client: Object,
  ) =>
    client.waitForVisible(STAKE_POOL_PAGE),
};
