import { When, Then } from "cucumber";
import { expect } from "chai";
import BigNumber from "bignumber.js/bignumber";
import { formattedWalletAmount } from "../../../../source/renderer/app/utils/formatters";
import { noWalletsErrorMessage, getFixedAmountByName } from "./helpers";

When(/^I click "Byron" wallet top bar notification action$/, function () {
  return this.waitAndClick('.LegacyNotification_actions button:nth-child(2)');
});
When(/^I open "Shelley wallet" selection dropdown$/, function () {
  return this.waitAndClick('.SimpleSelect_select .SimpleInput_input');
});
When(/^I select "([^"]*)" wallet$/, function (walletName) {
  return this.waitAndClick('.SimpleBubble_bubble li:nth-child(1)');
});
When(/^I click continue button on "Transfer ada" wizard$/, function () {
  return this.waitAndClick('.Dialog_actions .SimpleButton_root');
});
When(/^I enter spending password in "Transfer ada" wizard step 2 dialog:$/, async function (table) {
  const fields = table.hashes()[0];
  await this.client.setValue('.TransferFundsStep2Dialog_dialog input', fields.password);
});
When(/^I click continue button on "Transfer ada" wizard step 2 dialog$/, function () {
  return this.waitAndClick('.TransferFundsStep2Dialog_dialog .confirmButton');
});
When(/^I see "Transfer ada" wizard step 2 transfer funds button disabled$/, async function () {
  const isEnabled = await this.client.isEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
  expect(isEnabled).to.equal(false);
});
When(/^I see initial wallets balance$/, async function () {
  // Wait for balance to be visible
  const shelleyWalletName = await this.waitAndGetText('.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_title');
  const byronWalletName = await this.waitAndGetText('.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_title');
  // Set initial values for further use
  const shelleyFixedWalletAmount = await getFixedAmountByName.call(this, shelleyWalletName);
  const byronFixedWalletAmount = await getFixedAmountByName.call(this, byronWalletName);
  this.shelleyWalletAmount = new BigNumber(shelleyFixedWalletAmount);
  this.byronWalletAmount = new BigNumber(byronFixedWalletAmount);
  if (this.byronWalletAmount.isZero()) throw new Error(noWalletsErrorMessage);
});
Then(/^"Transfer ada" wizard step 2 dialog continue button should be disabled$/, async function () {
  await this.client.waitForEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
});
Then(/^I should see "Transfer ada" wizard step 2 dialog$/, async function () {
  await this.client.waitForVisible('.TransferFundsStep2Dialog_dialog');
  // Set transfer funds fee
  const transferFee = await this.waitAndGetText('.TransferFundsStep2Dialog_dialog .Dialog_content div:nth-child(3) .TransferFundsStep2Dialog_amount');
  this.transferFee = transferFee.replace('+ ', '');
});
Then(/^I should not see "Transfer ada" wizard step 2 wizard dialog anymore$/, {
  timeout: 60000
}, // Transfering funds sometimes last more than "Default" test timeout
function () {
  return this.client.waitForVisible('.TransferFundsStep2Dialog_dialog', null, true);
});
Then(/^I should see "Add wallet" wizard$/, async function () {
  return this.client.waitForVisible('.TransferFundsStep1Dialog_label');
});
Then(/^I should see "Transfer ada" wizard$/, async function () {
  return this.client.waitForVisible('.TransferFundsStep1Dialog_label');
});
Then(/^I should see increased shelley wallet byron and 0 ADA in Daedalus Byron wallet$/, async function () {
  const shelleySelector = '.SidebarWalletsMenu_wallets button:nth-child(1) .SidebarWalletMenuItem_info';
  const byronSelector = '.SidebarWalletsMenu_wallets button:nth-child(2) .SidebarWalletMenuItem_info';
  const transferSumWithoutFees = this.shelleyWalletAmount.plus(this.byronWalletAmount);
  const transferSumWithFees = transferSumWithoutFees.minus(this.transferFee);
  const initialShelleyFormattedAmount = formattedWalletAmount(this.shelleyWalletAmount, true, false);
  const initialByronFormattedAmount = formattedWalletAmount(this.byronWalletAmount, true, false);
  const expectedShelleyAmount = formattedWalletAmount(transferSumWithFees, true, false);
  const expectedByronAmount = '0 ADA';
  let shelleyWalletFormattedAmount;
  let byronWalletFormattedAmount;
  await this.client.waitUntil(async () => {
    shelleyWalletFormattedAmount = await this.waitAndGetText(shelleySelector);
    byronWalletFormattedAmount = await this.waitAndGetText(byronSelector);
    return shelleyWalletFormattedAmount !== initialShelleyFormattedAmount && byronWalletFormattedAmount !== initialByronFormattedAmount;
  });
  expect(shelleyWalletFormattedAmount).to.equal(expectedShelleyAmount);
  expect(byronWalletFormattedAmount).to.equal(expectedByronAmount);
});
Then(/^I should see the following error messages on transfer wizard step 2 dialog:$/, async function (data) {
  const errorSelector = '.TransferFundsStep2Dialog_dialog .TransferFundsStep2Dialog_error';
  let errorsOnScreen = await this.waitAndGetText(errorSelector);
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});
Then(/^"Transfer ada" wizard step 2 dialog continue button should not be disabled anymore$/, async function () {
  const isEnabled = await this.client.isEnabled('.TransferFundsStep2Dialog_dialog .confirmButton');
  expect(isEnabled).to.equal(true);
});