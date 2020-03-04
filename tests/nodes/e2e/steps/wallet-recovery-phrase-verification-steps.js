import { Given, When, Then } from 'cucumber';

const SETTINGS_PAGE_STATUS_SELECTOR = '.WalletRecoveryPhrase_validationStatus';
const SETTINGS_PAGE_BUTTON_SELECTOR = `${SETTINGS_PAGE_STATUS_SELECTOR} .WalletRecoveryPhrase_validationStatusButton`;
const DIALOG_SELECTOR = '.Dialog_dialogWrapper';
const DIALOG_CHECKBOX_SELECTOR = `${DIALOG_SELECTOR} .SimpleCheckbox_check`;
const DIALOG_CONTINUE_BUTTON_SELECTOR = `${DIALOG_SELECTOR} .SimpleButton_root`;
const DIALOG_SUCCESSFUL_SELECTOR = '.verification-successful';
const DIALOG_UNSUCCESSFUL_SELECTOR = '.verification-unsuccessful';
const DIALOG_VERIFY_AGAIN_BUTTON_SELECTOR = `${DIALOG_SELECTOR} button.attention`;
const DIALOG_CLOSE_BUTTON_SELECTOR = `${DIALOG_SELECTOR} .DialogCloseButton_component`;
const walletName = 'Wallet';

Given(
  'the last recovery phrase veryfication was done {int} days ago',
  async function(daysAgo) {
    await this.client.executeAsync((days, done) => {
      const { id } = daedalus.stores.wallets.active;
      const date = new Date();
      date.setDate(date.getDate() - days);
      const recoveryPhraseVerificationDate = date.toISOString();
      const { updateWalletLocalData } = daedalus.actions.wallets;
      updateWalletLocalData.once(done);
      updateWalletLocalData.trigger({
        id,
        recoveryPhraseVerificationDate,
      });
    }, daysAgo);
  }
);

Then(
  'I should see a {string} recovery phrase veryfication feature',
  async function(status) {
    const statusClassname = `${SETTINGS_PAGE_STATUS_SELECTOR}${status}`;
    return this.waitForVisible(statusClassname);
  }
);

When(/^I click the recovery phrase veryfication button$/, function() {
  return this.waitAndClick(SETTINGS_PAGE_BUTTON_SELECTOR);
});

When(/^I click the checkbox and Continue button$/, function() {
  this.waitAndClick(DIALOG_CHECKBOX_SELECTOR);
  return this.waitAndClick(DIALOG_CONTINUE_BUTTON_SELECTOR);
});

When(/^I enter the recovery phrase mnemonics correctly$/, async function() {
  const recoveryPhrase = this.mnemonics[walletName].slice();
  await this.client.executeAsync((phrase, done) => {
    const { checkRecoveryPhrase } = daedalus.actions.walletBackup;
    checkRecoveryPhrase.once(done);
    checkRecoveryPhrase.trigger({
      recoveryPhrase: phrase,
    });
  }, recoveryPhrase);
});

When(/^I enter the recovery phrase mnemonics incorrectly$/, async function() {
  const incorrectRecoveryPhrase = [...this.mnemonics[walletName]];
  incorrectRecoveryPhrase[0] = 'wrong';
  await this.client.executeAsync((phrase, done) => {
    const { checkRecoveryPhrase } = daedalus.actions.walletBackup;
    checkRecoveryPhrase.once(done);
    checkRecoveryPhrase.trigger({
      recoveryPhrase: phrase,
    });
  }, incorrectRecoveryPhrase);
});

When(/^I should see the confirmation dialog$/, async function() {
  return this.waitForVisible(DIALOG_SUCCESSFUL_SELECTOR);
});

When(/^I should see the error dialog$/, async function() {
  return this.waitForVisible(DIALOG_UNSUCCESSFUL_SELECTOR);
});

When(/^I should not see any dialog$/, async function() {
  return this.waitForVisible(DIALOG_SELECTOR, null, true);
});
When(/^I click the Verify again button$/, async function() {
  return this.waitAndClick(DIALOG_VERIFY_AGAIN_BUTTON_SELECTOR);
});
When(/^I click the close button$/, async function() {
  return this.waitAndClick(DIALOG_CLOSE_BUTTON_SELECTOR);
});
