// @flow
import { Given, When, Then } from 'cucumber';
import {
  isActiveWalletBeingRestored,
  waitForActiveRestoreNotification,
  addWalletPage,
  importWalletHelpers,
  i18n,
} from './helpers';
import { sidebarHelpers } from '../../../navigation/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;


When(/^I click on the import wallet button on the add wallet page/, function() {
  return addWalletPage.clickImportButton(this.client);
});

When(/^I see the import wallet dialog$/, function() {
  return importWalletHelpers.waitForDialog(this.client);
});

When(/^I select a valid wallet import key file$/, function() {
  this.waitAndClick('.WalletFileImportDialog .FileUploadWidget_dropZone');
});

When(/^I enter wallet spending password:$/, async function(table) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.WalletFileImportDialog .spendingPassword input',
    fields.password
  );
  await this.client.setValue(
    '.WalletFileImportDialog .repeatedPassword input',
    fields.repeatedPassword
  );
});

When(
  /^I click on the import wallet button in import wallet dialog$/,
  function() {
    return importWalletHelpers.clickImport(this.client);
  }
);

When(/^I should see wallet spending password inputs$/, function() {
  return this.waitForVisible(
    '.WalletFileImportDialog .spendingPassword input'
  );
});

When(/^I try to import the wallet with funds again$/, async function() {
  await sidebarHelpers.activateCategory(this.client, { category: 'wallets' });
  await sidebarHelpers.clickAddWalletButton(this.client);
  await addWalletPage.waitForVisible(this.client);
  await addWalletPage.clickImportButton(this.client);
  this.waitAndClick('.WalletFileImportDialog .FileUploadWidget_dropZone');
  this.waitAndClick('.Dialog_actions button');
});

Then(
  /^I see the import wallet dialog with an error that the wallet already exists$/,
  async function() {
    return importWalletHelpers.expectError(this.client, {
      error: await i18n.formatMessage(this.client, {
        id: 'api.errors.WalletAlreadyImportedError',
      }),
    });
  }
);

Then(/^I should not see the import wallet dialog anymore$/, function() {
  return importWalletHelpers.waitForDialog(this.client, { isHidden: true });
});

Then(
  /^I should see the restore status notification while import is running$/,
  async function() {
    // Only check the rendered DOM if the restore is still in progress
    if (await isActiveWalletBeingRestored(this.client)) {
      await waitForActiveRestoreNotification(this.client);
    }
  }
);

Then(
  /^I should not see the restore status notification once import is finished$/,
  async function() {
    await waitForActiveRestoreNotification(this.client, { isHidden: true });
  }
);
