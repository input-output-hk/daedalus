// @flow
import { When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';

import { restoreLegacyWallet, waitUntilWalletIsLoaded, addOrSetWalletsForScenario } from './helpers';

declare var daedalus: Daedalus;

When(/^I click "Balance" wallet top bar notification action$/, function() {
  return this.waitAndClick('.LegacyNotification_actions button:nth-child(2)');
});


When(/^I open "Rewards wallet" selection dropdown$/, function() {
  return this.waitAndClick(
    '.SimpleSelect_select .SimpleInput_input'
  );
});

When(/^I select "([^"]*)" wallet$/, function(walletName) {
  return this.waitAndClick('.SimpleBubble_bubble li:nth-child(1)');
});

When(/^I click continue button on "Transfer ada" wizard$/, function() {
  return this.waitAndClick('.Dialog_actions .SimpleButton_root');
});

When(/^I enter spending password in "Transfer ada" wizard step 2 dialog:$/, async function(
  table
) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.TransferFundsStep2Dialog_dialog input',
    fields.password
  );
});

When(/^I click continue button on "Transfer ada" wizard step 2 dialog$/, function() {
  return this.waitAndClick('.TransferFundsStep2Dialog_dialog .confirmButton');
});

When(/^I see "Transfer ada" wizard step 2 transfer funds button disabled and spinner$/, async function() {
  const isEnabled = await this.client.isEnabled('.TransferFundsStep2Dialog_submitButtonSpinning');
  expect(isEnabled).to.equal(false);
});

When(/^I see initial wallets balance$/, async function() {
  const rewardsWalletBalance = await this.client.getText('.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_info');
  expect(rewardsWalletBalance).to.equal('1M ADA');
  const balanceWalletBalance = await this.client.getText('.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_info');
  expect(balanceWalletBalance).to.equal('1M ADA');
});

When(/^I restore "([^"]*)" for transfer funds$/, async function(walletName) {
  await restoreLegacyWallet(this.client, { walletName, recoveryPhrase: ['collect', 'fold', 'file', 'clown', 'injury', 'sun', 'brass', 'diet', 'exist', 'spike', 'behave', 'clip'] });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Then(/^"Transfer ada" wizard step 2 dialog continue button should be disabled$/, async function() {
  await this.client.waitForEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
});

Then(/^I should see "Transfer ada" wizard step 2 dialog$/, function() {
  return this.client.waitForVisible('.TransferFundsStep2Dialog_dialog');
});

Then(
  /^I should not see "Transfer ada" wizard step 2 wizard dialog anymore$/,
  function() {
    return this.client.waitForVisible(
      '.TransferFundsStep2Dialog_dialog',
      null,
      true
    );
  }
);

Then(/^I should see "Add wallet" wizard$/, async function() {
  return this.client.waitForVisible('.TransferFundsStep1Dialog_label');
});

Then(/^I should see "Transfer ada" wizard$/, async function() {
  return this.client.waitForVisible('.TransferFundsStep1Dialog_label');
});

Then(
  /^I should see increased rewards wallet balance and 0 ADA in Daedalus Balance wallet$/,
  { timeout: 60000 },
  async function() {
    this.client.waitUntil(async () => {
      const rewardsWalletBalance = await this.client.getText('.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_info');
      const balanceWalletBalance = await this.client.getText('.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_info');
      return rewardsWalletBalance === '1.9M ADA' && balanceWalletBalance === '0 ADA';
    }, 60000);
  }
);

Then(
  /^I should see the following error messages on transfer wizard step 2 dialog:$/,
  async function(data) {
    const errorSelector = '.TransferFundsStep2Dialog_dialog .TransferFundsStep2Dialog_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.client.getText(errorSelector);
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i = 0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  }
);

Then(/^"Transfer ada" wizard step 2 dialog continue button should not be disabled anymore$/, async function() {
  const isEnabled = await this.client.isEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
  expect(isEnabled).to.equal(true);
});
