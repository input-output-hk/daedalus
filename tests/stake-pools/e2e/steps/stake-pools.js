// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';
import { createWallets, restoreWalletWithFunds, waitUntilWalletIsLoaded, addOrSetWalletsForScenario, addWalletPage } from '../../../wallets/e2e/steps/helpers';

declare var daedalus: Daedalus;

Given(/^I have a "([^"]*)" wallet with funds$/, async function (walletName) {
  await restoreWalletWithFunds(this.client, {walletName});
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Given(/^I have the following wallets:$/, async function (table) {
  await createWallets(table.hashes(), this);
});

Given(/^The sidebar shows "Delegation Center" page icon/, function () {
  return this.client.waitForVisible('.SidebarCategory_stakingIcon');
});

When(/^I click on the "Delegation Center" staking page button/, function() {
  return this.waitAndClick('.SidebarCategory_component.staking');
});

Then(/^I see the "Delegation Center" page$/, function() {
  return this.client.waitForVisible('.StakingWithNavigation_page', null, true);
});
