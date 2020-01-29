// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import {
  createWallets,
  getWalletByName,
  waitUntilWalletIsLoaded,
  addOrSetWalletsForScenario,
  restoreWalletWithFunds,
  restoreLegacyWallet,
  waitUntilUrlEquals,
  navigateTo,
} from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

// Create "Rewards" wallets
Given(/^I have the following "Rewards" wallets:$/, async function(table) {
  await createWallets(table.hashes(), this);
});

// Creates them sequentially
Given(/^I have created the following "Rewards" wallets:$/, async function(table) {
  await createWallets(table.hashes(), this, { sequentially: true });
});

Given(/^I have a "([^"]*)" rewards wallet with funds$/, async function(walletName) {
  await restoreWalletWithFunds(this.client, { walletName });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});



Given(/^I have the following legacy wallets:$/, async function(table) {
  await createWallets(table.hashes(), this, { isLegacy: true });
});

Given(/^I have the following wallets:$/, async function(table) {
  await createWallets(table.hashes(), this);
});

// Create "Balance" wallets
Given(/^I have a "([^"]*)" balance wallet$/, async function(walletName) {
  await restoreLegacyWallet(this.client, { walletName, hasFunds: false });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Given(/^I have a "([^"]*)" balance wallet with funds$/, async function(walletName) {
  await restoreLegacyWallet(this.client, { walletName, hasFunds: true });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Given(/^I am on the "([^"]*)" wallet "([^"]*)" screen$/, async function(
  walletName,
  screen
) {
  const wallet = getWalletByName.call(this, walletName);
  await navigateTo.call(this, `/wallets/${wallet.id}/${screen}`);
});

When(/^I have one wallet address$/, function() {
  return this.client.waitForVisible('.receiveAddress-1');
});

When(/^I enter spending password "([^"]*)"$/, function(password) {
  return this.client.setValue(
    '.WalletReceive_spendingPassword input',
    password
  );
});

When(/^I click the wallet (.*) button$/, async function(buttonName) {
  const buttonSelector = `.NavButton_component.${buttonName}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});

When(
  /^I submit the create wallet dialog with the following inputs:$/,
  async function(table) {
    const fields = table.hashes()[0];
    await this.client.setValue(
      '.WalletCreateDialog .walletName input',
      fields.walletName
    );
    return this.client.click('.WalletCreateDialog .primary');
  }
);

When(/^I click continue$/, function() {
  return this.waitAndClick('.primary');
});

When(/^I click close$/, function() {
  return this.waitAndClick('.primary');
});


Then(/^I should have newly created "([^"]*)" wallet loaded$/, async function(
  walletName
) {
  const result = await this.client.executeAsync(done => {
    daedalus.stores.wallets.walletsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  // Add or set the wallets for this scenario
  if (this.wallets != null) {
    this.wallets.push(...result.value);
  } else {
    this.wallets = result.value;
  }
  const wallet = getWalletByName.call(this, walletName);
  expect(wallet).to.be.an('object');
});

Then(/^I should be on some wallet page$/, async function() {
  return this.client.waitForVisible('.Navigation_component');
});

Then(/^I should be on the "([^"]*)" wallet "([^"]*)" screen$/, async function(
  walletName,
  screenName
) {
  const wallet = getWalletByName.call(this, walletName);
  return waitUntilUrlEquals.call(this, `/wallets/${wallet.id}/${screenName}`);
});

Then(/^I should be on the "([^"]*)" screen$/, async function(screenName) {
  return waitUntilUrlEquals.call(this, `/${screenName}`);
});

// Extended timeout is used for this step as it takes more than DEFAULT_TIMEOUT
// for the receiver wallet's balance to be updated on the backend after creating transactions
Then(
  /^the balance of "([^"]*)" wallet should be:$/,
  { timeout: 60000 },
  async function(walletName, table) {
    const expectedData = table.hashes()[0];
    const receiverWallet = getWalletByName.call(this, walletName);
    return this.client.waitUntil(async () => {
      const receiverWalletBalance = await this.client.getText(
        `.SidebarWalletsMenu_wallets .Wallet_${
          receiverWallet.id
        } .SidebarWalletMenuItem_info`
      );
      return receiverWalletBalance === `${expectedData.balance} ADA`;
    }, 60000);
  }
);

Then(
  /^"Balance" wallet badge should be visible in the wallet sidebar$/,
  async function() {
    return this.client.waitForVisible('.SidebarWalletMenuItem_active .LegacyBadge_component');
  }
);
