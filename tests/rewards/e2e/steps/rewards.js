// @flow
import { Given, When, Then } from 'cucumber';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const REWARDS_TAB_BUTTON = '.rewards.NavButton_component.NavButton_normal';
const REWARDS_PAGE = '.StakingRewards_component';
const NO_REWARDS_SELECTOR = '.StakingRewards_component .StakingRewards_noRewardsLabel';
const REWARDS_LIST_SELECTOR = '.StakingRewards_component .BorderedBox_component table';

When(/^I click on rewards tab button$/, async function () {
  return this.waitAndClick(REWARDS_TAB_BUTTON);
});

Then(/^I am on the rewards screen$/, async function () {
  await this.client.waitForVisible(REWARDS_PAGE);
});

Then(/^I should see rewards listed$/, async function () {
  await this.client.waitForVisible(REWARDS_LIST_SELECTOR);
});

Given(/^I set rewards as an empty result$/, async function () {
  await this.client.execute(() => {
    daedalus.api.ada.setTestingEmptyWallets();
  });
});

Then(/^I should see no rewards label$/, async function () {
  return this.client.waitForVisible(NO_REWARDS_SELECTOR);
});
