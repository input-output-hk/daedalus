import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { navigateTo } from '../helpers/route-helpers';
import {
  waitUntilWaletNamesEqual,
  getNameOfActiveWalletInSidebar,
} from '../helpers/wallets-helpers';

const SETTINGS_PAGE_STATUS_SELECTOR = '.WalletRecoveryPhrase_validationStatus';
const SETTINGS_PAGE_BUTTON_SELECTOR = `${SETTINGS_PAGE_STATUS_SELECTOR} .WalletRecoveryPhrase_validationStatusButton`;
const DIALOG_SELECTOR = '.Dialog_dialogWrapper';
const DIALOG_CHECKBOX_SELECTOR = `${DIALOG_SELECTOR} .SimpleCheckbox_check`;
const DIALOG_CONTINUE_BUTTON_SELECTOR = `${DIALOG_SELECTOR} .SimpleButton_root`;
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
    return await this.client.waitForVisible(statusClassname);
  }
);

When(/^I click the recovery phrase veryfication button$/, function() {
  return this.waitAndClick(SETTINGS_PAGE_BUTTON_SELECTOR);
});

When(/^I click the checkbox and Continue button$/, function() {
  this.waitAndClick(DIALOG_CHECKBOX_SELECTOR);
  return this.waitAndClick(DIALOG_CONTINUE_BUTTON_SELECTOR);
});

When(/^I enter the recovery phrase mnemonics correctly$/, function() {
  const recoveryPhrase = this.mnemonics[walletName];
  actions.walletBackup.checkRecoveryPhrase.trigger({
    recoveryPhrase,
  });
});

When(/^I enter the recovery phrase mnemonics incorrectly$/, function() {
  const recoveryPhrase = this.mnemonics[walletName];
  actions.walletBackup.checkRecoveryPhrase.trigger({
    recoveryPhrase,
  });
});
