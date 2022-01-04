import { Given, When, Then } from "cucumber";
import { addWalletPage } from "./helpers";

Given(/^I see the add wallet page/, function () {
  return addWalletPage.waitForVisible(this.client);
});
Given(/^I see the create wallet dialog$/, function () {
  return this.client.waitForVisible('.WalletCreateDialog');
});
Given(/^I dont see the create wallet dialog(?: anymore)?$/, function () {
  return this.client.waitForVisible('.WalletCreateDialog', null, true);
});
When(/^I click on the create wallet button on the add wallet page/, function () {
  return this.waitAndClick('.WalletAdd .createWalletButton');
});
When(/^I submit the create wallet with spending password dialog with the following inputs:$/, async function (table) {
  const fields = table.hashes()[0];
  await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
  await this.client.setValue('.WalletCreateDialog .spendingPassword input', fields.password);
  await this.client.setValue('.WalletCreateDialog .repeatedPassword input', fields.repeatedPassword);
  return this.waitAndClick('.WalletCreateDialog .primary');
});
When(/^I see the create wallet privacy dialog$/, function () {
  return this.client.waitForVisible('.WalletBackupPrivacyWarningDialog');
});
When(/^I click on "Please make sure nobody looks your screen" checkbox$/, function () {
  return this.waitAndClick('.WalletBackupPrivacyWarningDialog .SimpleCheckbox_root');
});
When(/^I submit the create wallet privacy dialog$/, function () {
  return this.waitAndClick('.WalletBackupPrivacyWarningDialog .primary');
});
When(/^I see the create wallet recovery phrase display dialog$/, function () {
  return this.client.waitForVisible('.WalletRecoveryPhraseDisplayDialog');
});
When(/^I note down the recovery phrase$/, async function () {
  const recoveryPhrase = await this.waitAndGetText('.WalletRecoveryPhraseMnemonic_component');
  this.recoveryPhrase = recoveryPhrase.split(' ');
});
When(/^I submit the create wallet recovery phrase display dialog$/, function () {
  return this.waitAndClick('.WalletRecoveryPhraseDisplayDialog .primary');
});
When(/^I see the create wallet recovery phrase entry dialog$/, function () {
  return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog');
});
When(/^I click on recovery phrase mnemonics in correct order$/, async function () {
  for (let i = 0; i < this.recoveryPhrase.length; i++) {
    const word = this.recoveryPhrase[i];
    const selector = 'MnemonicWord_root';
    const disabledSelector = 'MnemonicWord_disabled';
    await this.waitAndClick(`//button[contains(@class,'${selector}') and not(contains(@class, '${disabledSelector}')) and text()="${word}"]`);
  }
});
When(/^I click on the "Accept terms" checkboxes$/, async function () {
  const termsCheckboxes = await this.client.elements('.SimpleCheckbox_root');

  for (let i = 0; i < termsCheckboxes.value.length; i++) {
    const termsCheckbox = termsCheckboxes.value[i].ELEMENT;
    await this.client.elementIdClick(termsCheckbox);
  }
});
When(/^I submit the create wallet recovery phrase entry dialog$/, function () {
  return this.waitAndClick('.WalletRecoveryPhraseEntryDialog .primary');
});
Then(/^I should not see the create wallet recovery phrase entry dialog anymore$/, function () {
  return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog', null, true);
});