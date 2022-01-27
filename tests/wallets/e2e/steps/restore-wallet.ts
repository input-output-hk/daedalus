import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { isActiveWalletBeingRestored, waitForActiveRestoreNotification, getWalletByName } from "./helpers";
import { waitUntilTextInSelector, scrollIntoView } from "../../../common/e2e/steps/helpers";

Given(/^I see the restore wallet dialog$/, function () {
  return this.client.waitForVisible('.WalletRestoreDialog_component');
});
When(/^I click on the restore wallet button on the add wallet page$/, function () {
  return this.waitAndClick('.WalletAdd .restoreWalletButton');
});
When(/^I enter wallet name "([^"]*)" in restore wallet dialog$/, async function (walletName) {
  return this.client.setValue('.ConfigurationDialog_input.walletName input', walletName);
});
When(/^I clear the recovery phrase in restore wallet dialog$/, async function () {
  const words = await this.client.elements('.SimpleAutocomplete_selectedWordRemoveButton');

  for (let i = words.value.length - 1; i > -1; i--) {
    const wordId = words.value[i].ELEMENT;
    await this.client.elementIdClick(wordId);
  }
});
When(/^I enter recovery phrase in restore wallet dialog:$/, async function (table) {
  const fields = table.hashes()[0];
  const recoveryPhrase = fields.recoveryPhrase.split(' ');

  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.AutocompleteOverrides_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});
When(/^I enter wallet password in restore wallet dialog:$/, async function (table) {
  const fields = table.hashes()[0];
  await this.client.setValue('.spendingPassword input', fields.password);
  await this.client.setValue('.repeatPassword input', fields.repeatedPassword);
});
When(/^I submit the restore wallet dialog$/, function () {
  return this.client.click('.WalletRestoreDialog .primary');
});
Then(/^I should see section "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: '.WalletRestoreDialog_component .Dialog_content > div:last-child .RadioSet_label',
    text
  });
});
Then(/^I should not see the restore wallet dialog anymore$/, function () {
  return this.client.waitForVisible('.WalletRestoreDialog', null, true);
});
Then(/^I should see the restore status notification while restore is running$/, async function () {
  // Only check the rendered DOM if the restore is still in progress
  if (await isActiveWalletBeingRestored(this.client)) {
    await waitForActiveRestoreNotification(this.client);
  }
});
Then(/^I should not see the restore status notification once restore is finished$/, async function () {
  await waitForActiveRestoreNotification(this.client, {
    isHidden: true
  });
});
When(/^I click Check recovery phrase button$/, function () {
  return this.waitAndClick('.primary');
});
Then(/^I should see a screen titled "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Dialog_title h1',
    text,
    ignoreCase: true
  });
});
Then(/^I click on option "([^"]*)"$/, async function (text) {
  await this.waitAndClick(`//*[contains(text(), "${text}")]/..`);
});
Then(/^I confirm "([^"]*)"$/, async function (text) {
  const targetSelector = `//label[contains(text(), "${text}")]`;
  await this.client.waitForVisible(targetSelector);
  await scrollIntoView(this.client, targetSelector);
  await this.client.click(targetSelector);
});
Then(/^"([^"]*)" wallet should have "([^"]*)" as id$/, async function (walletName, walletId) {
  const wallet = await getWalletByName.call(this, walletName);
  expect(wallet.id).to.equal(walletId);
});
Given(/^I go back to the previous step$/, function () {
  return this.waitAndClick('.DialogBackButton_component');
});
Then(/^The error message should be (hidden|visible)$/, function (state) {
  const isVisible = state === 'visible';
  return this.client.waitForVisible('.ConfigurationDialog_error', null, !isVisible);
});