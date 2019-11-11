// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import BigNumber from 'bignumber.js';
import { DECIMAL_PLACES_IN_ADA } from '../../../../source/renderer/app/config/numbersConfig';
import {
  addWalletPage,
  importWalletHelpers,
  isActiveWalletBeingRestored,
  createWallets,
  fillOutWalletSendForm,
  getWalletByName,
  waitUntilWalletIsLoaded,
  addOrSetWalletsForScenario,
  restoreWalletWithFunds,
  waitUntilUrlEquals,
  navigateTo,
  sidebar,
  i18n,
  waitForActiveRestoreNotification,
} from './helpers';
import {
  sidebarHelpers,
} from '../../../navigation/e2e/steps/helpers';

Given(/^I have a "([^"]*)" wallet with funds$/, async function(walletName) {
  await restoreWalletWithFunds(this.client, { walletName });
  const wallet = await waitUntilWalletIsLoaded.call(this, walletName);
  addOrSetWalletsForScenario.call(this, wallet);
});

Given(/^I have the following wallets:$/, async function(table) {
  await createWallets(table.hashes(), this);
});

// Creates them sequentially
Given(/^I have created the following wallets:$/, async function(table) {
  await createWallets(table.hashes(), this, { sequentially: true });
});

Given(/^I am on the "([^"]*)" wallet "([^"]*)" screen$/, async function(
  walletName,
  screen
) {
  const wallet = getWalletByName.call(this, walletName);
  await navigateTo.call(this, `/wallets/${wallet.id}/${screen}`);
});

Given(/^I see the add wallet page/, function() {
  return addWalletPage.waitForVisible(this.client);
});

Given(/^I see delete wallet dialog$/, function() {
  return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog');
});

Given(/^I see the create wallet dialog$/, function() {
  return this.client.waitForVisible('.WalletCreateDialog');
});

Given(/^I see the restore wallet dialog$/, function() {
  return this.client.waitForVisible('.WalletRestoreDialog');
});

Given(/^I dont see the create wallet dialog(?: anymore)?$/, function() {
  return this.client.waitForVisible('.WalletCreateDialog', null, true);
});

When(/^I click on the create wallet button on the add wallet page/, function() {
  return this.waitAndClick('.WalletAdd .createWalletButton');
});

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
  return this.client.waitForVisible(
    '.WalletFileImportDialog .spendingPassword input'
  );
});

When(/^I have one wallet address$/, function() {
  return this.client.waitForVisible('.generatedAddress-1');
});

When(/^I enter spending password "([^"]*)"$/, function(password) {
  return this.client.setValue(
    '.WalletReceive_spendingPassword input',
    password
  );
});

When(
  /^I click on the restore wallet button on the add wallet page$/,
  function() {
    return this.waitAndClick('.WalletAdd .restoreWalletButton');
  }
);

When(/^I click the wallet (.*) button$/, async function(buttonName) {
  const buttonSelector = `.NavButton_component.${buttonName}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});

When(/^I can see the send form$/, function() {
  return this.client.waitForVisible('.WalletSendForm');
});

When(/^I fill out the wallet send form with:$/, function(table) {
  return fillOutWalletSendForm.call(this, table.hashes()[0]);
});

When(
  /^I fill out the send form with a transaction to "([^"]*)" wallet:$/,
  async function(walletName, table) {
    const values = table.hashes()[0];
    const walletId = getWalletByName.call(this, walletName).id;
    const walletAddress = await this.client.executeAsync((id, done) => {
      daedalus.api.ada
        .getAddresses({ walletId: id })
        .then(response => done(response[0].id))
        .catch(error => done(error));
    }, walletId);
    values.address = walletAddress.value;
    return fillOutWalletSendForm.call(this, values);
  }
);

When(/^the transaction fees are calculated$/, async function() {
  this.fees = await this.client.waitUntil(async () => {
    // Expected transactionFeeText format "+ 0.000001 of fees"
    const transactionFeeText = await this.client.getText(
      '.AmountInputSkin_fees'
    );
    const transactionFeeAmount = new BigNumber(transactionFeeText.substr(2, 8));
    return transactionFeeAmount.greaterThan(0) ? transactionFeeAmount : false;
  });
});

When(/^I click on the next button in the wallet send form$/, async function() {
  const submitButton = '.WalletSendForm_nextButton';
  await this.client.waitForVisible(submitButton);
  return this.client.click(submitButton);
});

When(/^I see send money confirmation dialog$/, function() {
  return this.client.waitForVisible('.WalletSendConfirmationDialog_dialog');
});

When(
  /^I enter wallet spending password in confirmation dialog "([^"]*)"$/,
  async function(password) {
    await this.client.setValue(
      '.WalletSendConfirmationDialog_passphrase input',
      password
    );
  }
);

When(/^I submit the wallet send form$/, async function() {
  await this.client.waitForEnabled(
    '.WalletSendConfirmationDialog_dialog .confirmButton'
  );
  return this.client.click(
    '.WalletSendConfirmationDialog_dialog .confirmButton'
  );
});

When(
  /^I submit the create wallet dialog with the following inputs:$/,
  async function(table) {
    const fields = table.hashes()[0];
    await this.client.setValue(
      '.WalletCreateDialog .walletName input',
      fields.walletName
    );
    return this.client.click('.WalletCreateDialog .primary');
  }
);

When(
  /^I submit the create wallet with spending password dialog with the following inputs:$/,
  async function(table) {
    const fields = table.hashes()[0];
    await this.client.setValue(
      '.WalletCreateDialog .walletName input',
      fields.walletName
    );
    await this.client.setValue(
      '.WalletCreateDialog .spendingPassword input',
      fields.password
    );
    await this.client.setValue(
      '.WalletCreateDialog .repeatedPassword input',
      fields.repeatedPassword
    );
    return this.client.click('.WalletCreateDialog .primary');
  }
);

When(/^I enter wallet name "([^"]*)" in restore wallet dialog$/, async function(
  walletName
) {
  return this.client.setValue(
    '.WalletRestoreDialog .walletName input',
    walletName
  );
});

When(/^I enter recovery phrase in restore wallet dialog:$/, async function(
  table
) {
  const fields = table.hashes()[0];
  const recoveryPhrase = fields.recoveryPhrase.split(' ');
  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue(
      '.AutocompleteOverrides_autocompleteWrapper input',
      word
    );
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});

When(/^I enter wallet password in restore wallet dialog:$/, async function(
  table
) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.WalletRestoreDialog .spendingPassword input',
    fields.password
  );
  await this.client.setValue(
    '.WalletRestoreDialog .repeatedPassword input',
    fields.repeatedPassword
  );
});

When(/^I submit the restore wallet dialog$/, function() {
  return this.client.click('.WalletRestoreDialog .primary');
});

When(/^I see the create wallet privacy dialog$/, function() {
  return this.client.waitForVisible('.WalletBackupPrivacyWarningDialog');
});

When(
  /^I click on "Please make sure nobody looks your screen" checkbox$/,
  function() {
    return this.waitAndClick(
      '.WalletBackupPrivacyWarningDialog .SimpleCheckbox_root'
    );
  }
);

When(/^I submit the create wallet privacy dialog$/, function() {
  return this.waitAndClick('.WalletBackupPrivacyWarningDialog .primary');
});

When(/^I see the create wallet recovery phrase display dialog$/, function() {
  return this.client.waitForVisible('.WalletRecoveryPhraseDisplayDialog');
});

When(/^I note down the recovery phrase$/, async function() {
  const recoveryPhrase = await this.client.getText(
    '.WalletRecoveryPhraseMnemonic_component'
  );
  this.recoveryPhrase = recoveryPhrase.split(' ');
});

When(/^I submit the create wallet recovery phrase display dialog$/, function() {
  return this.waitAndClick('.WalletRecoveryPhraseDisplayDialog .primary');
});

When(/^I see the create wallet recovery phrase entry dialog$/, function() {
  return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog');
});

When(
  /^I click on recovery phrase mnemonics in correct order$/,
  async function() {
    for (let i = 0; i < this.recoveryPhrase.length; i++) {
      const word = this.recoveryPhrase[i];
      const selector = 'MnemonicWord_root';
      const disabledSelector = 'MnemonicWord_disabled';
      await this.waitAndClick(
        `//button[contains(@class,'${selector}') and not(contains(@class, '${disabledSelector}')) and text()="${word}"]`
      );
    }
  }
);

When(/^I click on the "Accept terms" checkboxes$/, async function() {
  const termsCheckboxes = await this.client.elements('.SimpleCheckbox_root');
  for (let i = 0; i < termsCheckboxes.value.length; i++) {
    const termsCheckbox = termsCheckboxes.value[i].ELEMENT;
    await this.client.elementIdClick(termsCheckbox);
  }
});

When(/^I submit the create wallet recovery phrase entry dialog$/, function() {
  return this.waitAndClick('.WalletRecoveryPhraseEntryDialog .primary');
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

Then(
  /^I should not see the create wallet recovery phrase entry dialog anymore$/,
  function() {
    return this.client.waitForVisible(
      '.WalletRecoveryPhraseEntryDialog',
      null,
      true
    );
  }
);

Then(/^I should not see the delete wallet dialog anymore$/, function() {
  return this.client.waitForVisible(
    '.DeleteWalletConfirmationDialog_dialog',
    null,
    true
  );
});

Then(/^I should not see the import wallet dialog anymore$/, function() {
  return importWalletHelpers.waitForDialog(this.client, { isHidden: true });
});

Then(/^I should not see the restore wallet dialog anymore$/, function() {
  return this.client.waitForVisible('.WalletRestoreDialog', null, true);
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

Then(
  /^I should see the restore status notification while restore is running$/,
  async function() {
    // Only check the rendered DOM if the restore is still in progress
    if (await isActiveWalletBeingRestored(this.client)) {
      await waitForActiveRestoreNotification(this.client);
    }
  }
);

Then(
  /^I should not see the restore status notification once restore is finished$/,
  async function() {
    await waitForActiveRestoreNotification(this.client, { isHidden: true });
  }
);

Then(/^I should have newly created "([^"]*)" wallet loaded$/, async function(
  walletName
) {
  const result = await this.client.executeAsync(done => {
    daedalus.stores.wallets.walletsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  // Add or set the wallets for this scenario
  if (this.wallets != null) {
    this.wallets.push(...result.value);
  } else {
    this.wallets = result.value;
  }
  const wallet = getWalletByName.call(this, walletName);
  expect(wallet).to.be.an('object');
});

Then(/^I should be on some wallet page$/, async function() {
  return this.client.waitForVisible('.Navigation_component');
});

Then(/^I should be on the "([^"]*)" wallet "([^"]*)" screen$/, async function(
  walletName,
  screenName
) {
  const wallet = getWalletByName.call(this, walletName);
  return waitUntilUrlEquals.call(this, `/wallets/${wallet.id}/${screenName}`);
});

Then(/^I should be on the "([^"]*)" screen$/, async function(screenName) {
  return waitUntilUrlEquals.call(this, `/${screenName}`);
});

Then(
  /^I should see the following error messages on the wallet send form:$/,
  async function(data) {
    const errorSelector = '.WalletSendForm_component .SimpleFormField_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.client.getText(errorSelector);
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i = 0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  }
);

// TODO: refactor this to a less hackish solution (fees cannot easily be calculated atm)
Then(/^the latest transaction should show:$/, async function(table) {
  const expectedData = table.hashes()[0];
  await this.client.waitForVisible('.Transaction_title');
  let transactionTitles = await this.client.getText('.Transaction_title');
  transactionTitles = [].concat(transactionTitles);
  const expectedTransactionTitle = await this.intl(expectedData.title, {
    currency: 'Ada',
  });
  expect(expectedTransactionTitle).to.equal(transactionTitles[0]);
  let transactionAmounts = await this.client.getText('.Transaction_amount');
  transactionAmounts = [].concat(transactionAmounts);
  // Transaction amount includes transaction fees so we need to
  // substract them in order to get a match with expectedData.amountWithoutFees.
  // NOTE: we use "add()" as this is outgoing transaction and amount is a negative value!
  const transactionAmount = new BigNumber(transactionAmounts[0]);
  const transactionAmountWithoutFees = transactionAmount
    .add(this.fees)
    .toFormat(DECIMAL_PLACES_IN_ADA);
  expect(expectedData.amountWithoutFees).to.equal(transactionAmountWithoutFees);
});

// Extended timeout is used for this step as it takes more than DEFAULT_TIMEOUT
// for the receiver wallet's balance to be updated on the backend after creating transactions
Then(
  /^the balance of "([^"]*)" wallet should be:$/,
  { timeout: 60000 },
  async function(walletName, table) {
    const expectedData = table.hashes()[0];
    const receiverWallet = getWalletByName.call(this, walletName);
    return this.client.waitUntil(async () => {
      const receiverWalletBalance = await this.client.getText(
        `.SidebarWalletsMenu_wallets .Wallet_${
          receiverWallet.id
        } .SidebarWalletMenuItem_info`
      );
      return receiverWalletBalance === `${expectedData.balance} ADA`;
    }, 60000);
  }
);

Then(
  /^I should see newly generated address as active address on the wallet receive screen$/,
  async function() {
    return this.client.waitUntil(async () => {
      const activeAddress = await this.client.getText('.WalletReceive_hash');
      const generatedAddress = await this.client.getText(
        '.generatedAddress-1 .Address_addressId'
      );
      return generatedAddress === activeAddress;
    });
  }
);

Then(/^I should see the wallets in the following order:$/, async function(
  table
) {
  const expectedWallets = table.hashes();
  const wallets = await this.client.getText('.SidebarWalletMenuItem_title');
  wallets.forEach((wallet, index) =>
    expect(wallet).to.equal(expectedWallets[index].name)
  );
});
