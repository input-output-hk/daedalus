// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { delegationCentreStakingHelper } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const {
  stakingButtonVisible,
  clickStakingButton,
  delegationCenterVisible,
} = delegationCentreStakingHelper;

const LOADING_SPINNER_SELECTOR = '.StakingRewardsForIncentivizedTestnet_component .StakingRewardsForIncentivizedTestnet_loadingSpinnerWrapper';
const REWARDS_TAB_BUTTON = '.rewards.NavButton_component.NavButton_normal';
const REWARDS_PAGE = '.StakingRewardsForIncentivizedTestnet_component';
const REWARDS_LIST_SELECTOR = '.StakingRewardsForIncentivizedTestnet_component .BorderedBox_component table';

Given(/^I am on the Delegation Center screen/, async function () {
  await stakingButtonVisible(this.client);
  await clickStakingButton(this.client);
  await delegationCenterVisible(this.client);
});

When(/^I mark experimental feature as read/, async function () {
  await this.client.waitForVisible('.ExperimentalDataOverlay_component');
  return this.waitAndClick('.ExperimentalDataOverlay_actionButton');
});

Then(/^I click on rewards tab button/, async function () {
  return this.waitAndClick(REWARDS_TAB_BUTTON);
});

Then(/^I am on the rewards screen/, async function () {
  await this.client.waitForVisible(REWARDS_PAGE);
});

Then(/^I should't see loading spinner anymore/, function () {
  return this.client.waitForVisible(LOADING_SPINNER_SELECTOR, null, true);
});

Then(/^I should see rewards listed/, async function () {
  await this.client.waitForVisible(REWARDS_LIST_SELECTOR);
});

Then(/^I should not see any rewards$/, async function () {
  return this.client.waitForVisible(REWARDS_LIST_SELECTOR, null, true);
});
