// @flow
import { When, Then } from 'cucumber';
import { expect } from 'chai';
import { get } from 'lodash'
import BigNumber from 'bignumber.js/bignumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import type { Daedalus } from '../../../types';

import { noWalletsErrorMessage, getWalletByName, getFixedAmountByName } from './helpers';

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
  const rewardsFixedWalletAmount = await getFixedAmountByName.call(this, rewardsWalletName);
  const balanceFixedWalletAmount = await getFixedAmountByName.call(this, balanceWalletName);
  const rewardsWalletAmount = new BigNumber(rewardsFixedWalletAmount);
  const balanceWalletAmount = new BigNumber(balanceFixedWalletAmount);
  if (balanceWalletAmount.isZero()) throw new Error(noWalletsErrorMessage);
  this.rewardsWalletAmount = rewardsWalletAmount;
  this.balanceWalletAmount = balanceWalletAmount;
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
    const rewardsSelector = '.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_info';
    const balanceSelector = '.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_info';
    const transferSumWithoutFees = this.rewardsWalletAmount.add(this.balanceWalletAmount);
    const transferSumWithFees = transferSumWithoutFees.minus(this.transferFee);
    const initialRewardsFormattedAmount = formattedWalletAmount(this.rewardsWalletAmount, true, false);
    const initialBallanceFormattedAmount = formattedWalletAmount(this.balanceWalletAmount, true, false);
    const expectedRewardsAmount = formattedWalletAmount(transferSumWithFees, true, false);
    const expectedBalanceAmount = '0 ADA';
    let rewardsWalletFormattedAmount;
    let balanceWalletFormattedAmount;
    await this.client.waitUntil(async () => {
      rewardsWalletFormattedAmount = await this.waitAndGetText(rewardsSelector);
      balanceWalletFormattedAmount = await this.waitAndGetText(balanceSelector);
      return(
        rewardsWalletFormattedAmount !== initialRewardsFormattedAmount &&
        balanceWalletFormattedAmount !== initialBallanceFormattedAmount
      );
    })
    expect(rewardsWalletFormattedAmount).to.equal(expectedRewardsAmount);
    expect(balanceWalletFormattedAmount).to.equal(expectedBalanceAmount);
  }
);

Then(
  /^I should see the following error messages on transfer wizard step 2 dialog:$/,
  async function(data) {
    const errorSelector = '.TransferFundsStep2Dialog_dialog .TransferFundsStep2Dialog_error';
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
