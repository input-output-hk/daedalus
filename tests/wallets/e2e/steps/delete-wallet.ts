import { Given, When, Then } from "cucumber";

Given(/^I see delete wallet dialog$/, function () {
  return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog');
});
When(/^I click on delete wallet button$/, async function () {
  return this.waitAndClick('.WalletSettings_deleteWalletBox button');
});
When(/^I enter "([^"]*)" as name of the wallet to confirm$/, async function (walletName) {
  return this.client.setValue('.DeleteWalletConfirmationDialog_confirmationInput input', walletName);
});
When(/^I click on the "Make sure you have access to backup before continuing" checkbox$/, function () {
  return this.waitAndClick('.DeleteWalletConfirmationDialog_dialog .SimpleCheckbox_root');
});
When(/^I submit the delete wallet dialog$/, function () {
  return this.client.click('.DeleteWalletConfirmationDialog_dialog .primary');
});
Then(/^I should not see the delete wallet dialog anymore$/, function () {
  return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog', null, true);
});