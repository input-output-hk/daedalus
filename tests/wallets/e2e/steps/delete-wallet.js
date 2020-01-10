// @flow
import { Given, When, Then } from 'cucumber';
import {
  isActiveWalletBeingRestored,
  waitUntilWalletIsLoaded,
  addOrSetWalletsForScenario,
  restoreWalletWithFunds,
  restoreLegacyWallet,
  waitForActiveRestoreNotification,
} from './helpers';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I see delete wallet dialog$/, function() {
  return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog');
});

When(/^I click on delete wallet button$/, async function() {
  return this.waitAndClick('.WalletSettings_deleteWalletBox button');
});

When(/^I enter "([^"]*)" as name of the wallet to confirm$/, async function(
  walletName
) {
  return this.client.setValue(
    '.DeleteWalletConfirmationDialog_confirmationInput input',
    walletName
  );
});

When(
  /^I click on the "Make sure you have access to backup before continuing" checkbox$/,
  function() {
    return this.waitAndClick(
      '.DeleteWalletConfirmationDialog_dialog .SimpleCheckbox_root'
    );
  }
);

When(/^I submit the delete wallet dialog$/, function() {
  return this.client.click('.DeleteWalletConfirmationDialog_dialog .primary');
});

Then(/^I should not see the delete wallet dialog anymore$/, function() {
  return this.client.waitForVisible(
    '.DeleteWalletConfirmationDialog_dialog',
    null,
    true
  );
});