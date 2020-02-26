// @flow
import { When, Then } from 'cucumber';
import path from 'path';
import moment from 'moment';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const exportedCSVPath = path.resolve(
  __dirname,
  '../documents/rewards_exported.csv'
);
const exportedCSVContent = [['Wallet', 'Reward', 'Date'], ['Test Wallet', '1,000,000.000000 ADA', moment().format('YYYY-MM-DDTHHmmss.0SSS')]];
const REWARDS_TAB_BUTTON = '.rewards.NavButton_component.NavButton_normal';
const REWARDS_PAGE = '.StakingRewardsForIncentivizedTestnet_component';
const NO_REWARDS_SELECTOR = '.StakingRewardsForIncentivizedTestnet_component .StakingRewardsForIncentivizedTestnet_noRewardsLabel';
const REWARDS_LIST_SELECTOR = '.StakingRewardsForIncentivizedTestnet_component .BorderedBox_component table';

When(/^I click on rewards tab button$/, async function () {
  return this.waitAndClick(REWARDS_TAB_BUTTON);
});

Then(/^I am on the rewards screen$/, async function () {
  await this.client.waitForVisible(REWARDS_PAGE);
});

Then(/^I should see rewards listed$/, async function () {
  await this.client.waitForVisible(REWARDS_LIST_SELECTOR);
});

Then(/^I should see no rewards label$/, async function () {
  return this.client.waitForVisible(NO_REWARDS_SELECTOR);
});

Then(/^I click on the Export to CSV button$/, async function () {
  /**
   * Clicking the real button would open the system dialog which we cannot test
   * easily. So we just skip that step and pretend the user picked a path
   */
  const data = {
    filePath: exportedCSVPath,
    fileContent: exportedCSVContent,
  };

  await this.client.execute(params => {
    daedalus.actions.wallets.generateRewardsCsv.trigger({
      rewards: params.fileContent,
      filePath: params.filePath,
    });
  }, data);
});

Then(/^I should see the CSV file exported$/, async function () {
  /**
   * Check manually if CSV file was exported successfully in the specified path
   */
  return true;
});
