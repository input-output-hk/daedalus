import { Given, When, Then } from "cucumber";

const SETTINGS_PAGE_STATUS_SELECTOR = '.WalletRecoveryPhraseVerificationWidget_status';
const SETTINGS_PAGE_BUTTON_SELECTOR = `${SETTINGS_PAGE_STATUS_SELECTOR} .WalletRecoveryPhraseVerificationWidget_statusButton`;
const DIALOG_SELECTOR = '.Dialog_dialogWrapper';
const DIALOG_CHECKBOX_SELECTOR = `${DIALOG_SELECTOR} .SimpleCheckbox_check`;
const DIALOG_CONTINUE_BUTTON_SELECTOR = `${DIALOG_SELECTOR} .SimpleButton_root`;
const DIALOG_SUCCESSFUL_SELECTOR = '.verification-successful';
const DIALOG_UNSUCCESSFUL_SELECTOR = '.verification-unsuccessful';
const DIALOG_VERIFY_AGAIN_BUTTON_SELECTOR = `${DIALOG_SELECTOR} button.attention`;
const DIALOG_CLOSE_BUTTON_SELECTOR = `${DIALOG_SELECTOR} .DialogCloseButton_component`;
const DIALOG_VERIFY_BUTTON_SELECTOR = '.WalletRecoveryPhraseStepDialogs_dialog button.primary';
Given('the last recovery phrase veryfication was done {int} days ago', async function (daysAgo) {
  await this.client.executeAsync((days, done) => {
    const {
      id: walletId
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus.stores.wallets.active;
    const date = new Date();
    date.setDate(date.getDate() - days);
    const recoveryPhraseVerificationDate = date.toISOString();
    const {
      setWalletLocalData
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus.actions.walletsLocal;
    setWalletLocalData.once(done);
    setWalletLocalData.trigger({
      walletId,
      updatedWalletData: {
        recoveryPhraseVerificationDate
      }
    });
  }, daysAgo);
});
Then('I should see a {string} recovery phrase veryfication feature', async function (status) {
  const statusClassname = `${SETTINGS_PAGE_STATUS_SELECTOR}${status}`;
  return this.client.waitForVisible(statusClassname);
});
When(/^I click the recovery phrase veryfication button$/, function () {
  return this.waitAndClick(SETTINGS_PAGE_BUTTON_SELECTOR);
});
When(/^I click the checkbox and Continue button$/, function () {
  this.waitAndClick(DIALOG_CHECKBOX_SELECTOR);
  return this.waitAndClick(DIALOG_CONTINUE_BUTTON_SELECTOR);
});
When(/^I enter the recovery phrase mnemonics (correctly|incorrectly)$/, async function (_type) {
  const recoveryPhrase = await this.client.execute((type, mnemonics) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const activeWallet = daedalus.stores.wallets.active;
    const correctMnemonics = mnemonics[activeWallet.name];
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const incorrectMnemonics = daedalus.utils.crypto.generateMnemonic(correctMnemonics.length).split(' ');
    return type === 'correctly' ? correctMnemonics : incorrectMnemonics;
  }, _type, this.mnemonics);

  for (let i = 0; i < recoveryPhrase.value.length; i++) {
    const word = recoveryPhrase.value[i];
    await this.client.setValue('.AutocompleteOverrides_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});
When(/^I enter the "([^"]*)" recovery phrase mnemonics$/, async function (_recoveryPhrase) {
  const recoveryPhrase = _recoveryPhrase.split(' ');

  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.AutocompleteOverrides_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});
When(/^I click the verify button$/, async function () {
  await this.client.waitForEnabled(DIALOG_VERIFY_BUTTON_SELECTOR);
  return this.waitAndClick(DIALOG_VERIFY_BUTTON_SELECTOR);
});
When(/^I should see the confirmation dialog$/, async function () {
  return this.client.waitForVisible(DIALOG_SUCCESSFUL_SELECTOR);
});
When(/^I should see the error dialog$/, async function () {
  return this.client.waitForVisible(DIALOG_UNSUCCESSFUL_SELECTOR);
});
When(/^I should not see any dialog$/, async function () {
  return this.client.waitForVisible(DIALOG_SELECTOR, null, true);
});
When(/^I click the Verify again button$/, async function () {
  return this.waitAndClick(DIALOG_VERIFY_AGAIN_BUTTON_SELECTOR);
});
When(/^I click the close button$/, async function () {
  return this.waitAndClick(DIALOG_CLOSE_BUTTON_SELECTOR);
});