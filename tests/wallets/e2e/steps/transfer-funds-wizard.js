// @flow
import { When, Then } from 'cucumber';
import { expect } from 'chai';
import { get } from 'lodash'
import BigNumber from 'bignumber.js/bignumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import type { Daedalus } from '../../../types';

import { restoreLegacyWallet, waitUntilWalletIsLoaded, addOrSetWalletsForScenario, getWalletByName } from './helpers';

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
  // Wait for balance to be visible
  const rewardsWalletName = await this.waitAndGetText('.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_title');
  const balanceWalletName = await this.waitAndGetText('.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_title');

  // Set initial values for further use
  const rewardsWallet = getWalletByName.call(this, rewardsWalletName);
  const balanceWallet = getWalletByName.call(this, balanceWalletName);
  const rewardsWalletAmount = get(rewardsWallet, 'amount.c', [0]);
  const balanceWalletAmount = get(balanceWallet, 'amount.c', [0]);
  this.rewardsWalletBalance = new BigNumber(rewardsWalletAmount);
  this.balanceWalletBalance = new BigNumber(balanceWalletAmount);
});

When(/^I restore "([^"]*)" for transfer funds$/, async function(walletName) {
  await restoreLegacyWallet(this.client, { walletName, recoveryPhrase: ['collect', 'fold', 'file', 'clown', 'injury', 'sun', 'brass', 'diet', 'exist', 'spike', 'behave', 'clip'] });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Then(/^"Transfer ada" wizard step 2 dialog continue button should be disabled$/, async function() {
  await this.client.waitForEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
});

Then(/^I should see "Transfer ada" wizard step 2 dialog$/, async function() {
  await this.client.waitForVisible('.TransferFundsStep2Dialog_dialog');
  // Set transfer funds fee
  const transferFee = await this.waitAndGetText('.TransferFundsStep2Dialog_dialog .Dialog_content div:nth-child(3) .TransferFundsStep2Dialog_amount');
  this.transferFee = transferFee.replace('+ ', '');
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


Then(/^I should see increased rewards wallet balance and 0 ADA in Daedalus Balance wallet$/,
  async function() {
    const transferSumWithoutFees = this.rewardsWalletBalance.add(this.balanceWalletBalance);
    const transferSumWithFees = transferSumWithoutFees.minus(this.transferFee);
    const formattedTransferSum = formattedWalletAmount(transferSumWithFees, true, false);
    await waitUntilTextInSelector(this.client, {
      selector: '.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_info',
      text: formattedTransferSum,
    });
    await waitUntilTextInSelector(this.client, {
      selector: '.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_info',
      text: '0 ADA',
    });
  }
);

Then(
  /^I should see the following error messages on transfer wizard step 2 dialog:$/,
  async function(data) {
    const errorSelector = '.TransferFundsStep2Dialog_dialog .TransferFundsStep2Dialog_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.waitAndGetText(errorSelector);
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
