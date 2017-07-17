import { expect } from 'chai';
import {
  fillOutWalletSendForm,
  getWalletByName,
} from './lib/wallets-helpers';
import {
  waitUntilUrlEquals,
} from './lib/route-helpers'
import path from 'path';

const defaultWalletKeyFilePath = path.resolve(__dirname, '../support/default-wallet.key');

export default function () {

  this.Given(/^I have a wallet with funds$/, async function () {
    const result = await this.client.executeAsync(function(filePath, done) {
      // This assumes that we always have a default wallet on the backend!
      daedalus.api.importWalletFromKey({ filePath, walletPassword: null }).then((wallet) => {
        daedalus.stores.wallets.refreshWalletsData().then(() => done(wallet));
      });
    }, defaultWalletKeyFilePath);
    this.wallet = result.value;
    // Add or set the wallets for this scenario
    if (this.wallets != null) {
      this.wallets.push(this.wallet);
    } else {
      this.wallets = [this.wallet];
    }
  });

  this.Given(/^I have a wallet with funds and password$/, async function () {
    const result = await this.client.executeAsync(function(filePath, done) {
      // This assumes that we always have a default wallet on the backend!
      daedalus.api.importWalletFromKey({ filePath, walletPassword: 'Secret123' }).then((wallet) => {
        daedalus.stores.wallets.refreshWalletsData().then(() => done(wallet));
      });
    }, defaultWalletKeyFilePath);
    this.wallet = result.value;
    // Add or set the wallets for this scenario
    if (this.wallets != null) {
      this.wallets.push(this.wallet);
    } else {
      this.wallets = [this.wallet];
    }
  });

  this.Given(/^I have the following wallets:$/, async function (table) {
    const result = await this.client.executeAsync(function(wallets, done) {
      window.Promise.all(wallets.map((wallet) => {
        return daedalus.api.createWallet({
          name: wallet.name,
          mnemonic: daedalus.api.generateMnemonic().join(' '),
          password: wallet.password || null,
        });
      }))
      .then(() => {
        daedalus.stores.wallets.walletsRequest.execute().then(done);
      })
      .catch((error) => done(error.stack));
    }, table.hashes());
    // Add or set the wallets for this scenario
    if (this.wallets != null) {
      this.wallets.push(...result.value);
    } else {
      this.wallets = result.value;
    }
  });

  this.Given(/^I am on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screen) {
    const wallet = getWalletByName.call(this, walletName);
    await this.navigateTo(`/wallets/${wallet.id}/${screen}`);
  });

  this.Given(/^I see the add wallet dialog$/, function () {
    return this.client.waitForVisible('.WalletAddDialog');
  });

  this.Given(/^I see delete wallet dialog$/, function () {
    return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog');
  });

  this.Given(/^I see the create wallet dialog$/, function () {
    return this.client.waitForVisible('.WalletCreateDialog');
  });

  this.Given(/^I see the restore wallet dialog$/, function () {
    return this.client.waitForVisible('.WalletRestoreDialog');
  });

  this.Given(/^I dont see the create wallet dialog(?: anymore)?$/, function () {
    return this.client.waitForVisible('.WalletCreateDialog', null, true);
  });

  this.Given(/^the active wallet is "([^"]*)"$/, function (walletName) {
    const wallet = getWalletByName.call(this, walletName);
    this.client.execute(walletId => {
      daedalus.actions.setActiveWallet.trigger({ walletId });
    }, wallet.id);
  });

  this.When(/^I click on the create wallet button in add wallet dialog$/, function () {
    return this.waitAndClick('.WalletAddDialog .createWalletButton');
  });

  this.When(/^I click on the import wallet button in add wallet dialog$/, function () {
    return this.waitAndClick('.WalletAddDialog .importWalletButton');
  });

  this.When(/^I see the import wallet dialog$/, function () {
    return this.client.waitForVisible('.WalletKeyImportDialog');
  });

  this.When(/^I select a valid wallet import key file$/, async function () {
    await this.client.chooseFile('.WalletKeyImportDialog_keyUpload .FileUploadWidget_dropZone input', defaultWalletKeyFilePath);
  });

  this.When(/^I toggle "Activate to create password" switch on the import wallet key dialog$/, function () {
    return this.waitAndClick('.WalletKeyImportDialog .switch_field');
  });

  this.When(/^I enter wallet spending password:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletKeyImportDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletKeyImportDialog .repeatedPassword input', fields.repeatedPassword);
  });

  this.When(/^I click on the import wallet button in import wallet dialog$/, function () {
    return this.waitAndClick('.WalletKeyImportDialog .dialog_button');
  });

  this.When(/^I should see wallet spending password inputs$/, function () {
    return this.client.waitForVisible('.WalletKeyImportDialog .walletPassword input');
  });

  this.When(/^I have one wallet address$/, function () {
    return this.client.waitForVisible('.generatedAddress-1');
  });

  this.When(/^I enter spending password "([^"]*)"$/, function (password) {
    return this.client.setValue('.WalletReceive_spendingPassword input', password);
  });

  this.When(/^I click on the "Generate new address" button$/, function () {
    return this.client.click('.generateAddressButton');
  });

  this.When(/^I click on the restore wallet button in add wallet dialog$/, function () {
    return this.waitAndClick('.WalletAddDialog .restoreWalletButton');
  });

  this.When(/^I click the wallet (.*) button$/, async function (buttonName) {
    const buttonSelector = `.WalletNavButton_component.${buttonName}`;
    await this.client.waitForVisible(buttonSelector);
    await this.client.click(buttonSelector);
  });

  this.When(/^I fill out the wallet send form with:$/, function (table) {
    return fillOutWalletSendForm.call(this, table.hashes()[0]);
  });

  this.When(/^I fill out the send form with a transaction to "([^"]*)" wallet:$/, async function (walletName, table) {
    const values = table.hashes()[0];
    const walletId = this.wallets.find((w) => w.name === walletName).id;
    const walletAddress = await this.client.executeAsync(function(id, done) {
      daedalus.api.getAddresses({ walletId: id }).then((response) => {
        const address = response.addresses[0].id;
        done(address);
      });
    }, walletId);
    values.address = walletAddress.value;
    return fillOutWalletSendForm.call(this, values);
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.When(/^I toggle "Activate to create password" switch on the create wallet dialog$/, function () {
    return this.waitAndClick('.WalletCreateDialog .switch_field');
  });

  this.When(/^I toggle "Activate to create password" switch on the restore wallet dialog$/, function () {
    return this.waitAndClick('.WalletRestoreDialog .switch_field');
  });

  this.When(/^I submit the create wallet dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
    return this.client.click('.WalletCreateDialog .dialog_button');
  });

  this.When(/^I submit the create wallet with spending password dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletCreateDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletCreateDialog .repeatedPassword input', fields.repeatedPassword);
    return this.client.click('.WalletCreateDialog .dialog_button');
  });

  this.When(/^I submit the restore wallet dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletRestoreDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletRestoreDialog .recoveryPhrase textarea', fields.recoveryPhrase);
    return this.client.click('.WalletRestoreDialog .dialog_button');
  });

  this.When(/^I submit the restore wallet with spending password dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletRestoreDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletRestoreDialog .recoveryPhrase textarea', fields.recoveryPhrase);
    await this.client.setValue('.WalletRestoreDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletRestoreDialog .repeatedPassword input', fields.repeatedPassword);
    return this.client.click('.WalletRestoreDialog .dialog_button');
  });

  this.When(/^I see the create wallet privacy dialog$/, function () {
    return this.client.waitForVisible('.WalletBackupPrivacyWarningDialog');
  });

  this.When(/^I click on "Please make sure nobody looks your screen" checkbox$/, function () {
    return this.waitAndClick('.WalletBackupPrivacyWarningDialog .CheckboxWithLongLabel_checkbox');
  });

  this.When(/^I submit the create wallet privacy dialog$/, function () {
    return this.waitAndClick('.WalletBackupPrivacyWarningDialog .dialog_button');
  });

  this.When(/^I see the create wallet recovery phrase display dialog$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseDisplayDialog');
  });

  this.When(/^I note down the recovery phrase$/, async function () {
    const recoveryPhrase = await this.client.getText('.WalletRecoveryPhraseMnemonic_component');
    this.recoveryPhrase = recoveryPhrase.split(' ');
  });

  this.When(/^I submit the create wallet recovery phrase display dialog$/, function () {
    return this.waitAndClick('.WalletRecoveryPhraseDisplayDialog .dialog_button');
  });

  this.When(/^I see the create wallet recovery phrase entry dialog$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog');
  });

  this.When(/^I click on recovery phrase mnemonics in correct order$/, async function () {
    for (let i = 0; i < this.recoveryPhrase.length; i++) {
      const recoveryPhraseMnemonic = this.recoveryPhrase[i];
      await this.waitAndClick(`//button[contains(text(), "${recoveryPhraseMnemonic}") and @class="MnemonicWord_component MnemonicWord_active"]`);
    }
  });

  this.When(/^I click on the "Accept terms" checkboxes$/, async function () {
    const termsCheckboxes = await this.client.elements('.CheckboxWithLongLabel_checkbox');
    for (let i = 0; i < termsCheckboxes.value.length; i++) {
      const termsCheckbox = termsCheckboxes.value[i].ELEMENT;
      await this.client.elementIdClick(termsCheckbox);
    }
  });

  this.When(/^I submit the create wallet recovery phrase entry dialog$/, function () {
    return this.waitAndClick('.WalletRecoveryPhraseEntryDialog .dialog_button');
  });

  this.When(/^I click on delete wallet button$/, async function () {
    return this.client.click('.DeleteWalletButton_button');
  });

  this.When(/^I enter "([^"]*)" as name of the wallet to confirm$/, async function (walletName) {
    return this.client.setValue('.DeleteWalletConfirmationDialog_confirmationInput input', walletName);
  });

  this.When(/^I click on the "Make sure you have access to backup before continuing" checkbox$/, function () {
    return this.waitAndClick('.DeleteWalletConfirmationDialog_dialog .CheckboxWithLongLabel_checkbox');
  });

  this.When(/^I submit the delete wallet dialog$/, async function () {
    return this.client.click('.DeleteWalletConfirmationDialog_dialog .button_primary');
  });

  this.Then(/^I should not see the create wallet recovery phrase entry dialog anymore$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog', null, true);
  });

  this.Then(/^I should not see the delete wallet dialog anymore$/, function () {
    return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog', null, true);
  });

  this.Then(/^I should not see the import wallet dialog anymore$/, function () {
    return this.client.waitForVisible('.WalletKeyImportDialog', null, true);
  });

  this.Then(/^I should not see the restore wallet dialog anymore$/, function () {
    return this.client.waitForVisible('.WalletRestoreDialog', null, true);
  });

  this.Then(/^I should have newly created "([^"]*)" wallet loaded$/, async function (walletName) {
    const result = await this.client.executeAsync(function(done) {
      daedalus.stores.wallets.walletsRequest.execute().then(done);
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

  this.Then(/^I should be on some wallet page$/, async function () {
    return this.client.waitForVisible('.WalletNavigation_component');
  });

  this.Then(/^I should be on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screenName) {
    const wallet = getWalletByName.call(this, walletName);
    return waitUntilUrlEquals.call(this, `/wallets/${wallet.id}/${screenName}`);
  });

  this.Then(/^I should see the following error messages on the wallet send form:$/, async function (data) {
    const errorSelector = '.WalletSendForm_component .SimpleFormField_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.client.getText(errorSelector);
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i = 0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  });

  this.Then(/^the latest transaction should show:$/, async function (table) {
    const expectedData = table.hashes()[0];
    await this.client.waitForVisible('.Transaction_title');
    let transactionTitles = await this.client.getText('.Transaction_title');
    transactionTitles = [].concat(transactionTitles);
    const expectedTransactionTitle = await this.intl(expectedData.title);
    expect(expectedTransactionTitle).to.equal(transactionTitles[0]);
    let transactionAmounts = await this.client.getText('.Transaction_amount');
    transactionAmounts = [].concat(transactionAmounts);
    expect(expectedData.amount).to.equal(transactionAmounts[0]);
  });

  this.Then(/^the balance of "([^"]*)" wallet should be:$/, async function (walletName, table) {
    const expectedData = table.hashes()[0];
    const receiverWallet = getWalletByName.call(this, walletName);
    const receiverWalletBalance = await this.client.getText(`.SidebarWalletsMenu_wallets .Wallet_${receiverWallet.id} .SidebarWalletMenuItem_info`);
    expect(receiverWalletBalance).to.equal(`${expectedData.balance} ADA`);
  });

  this.Then(/^I should see newly generated address as active address on the wallet receive screen$/, async function () {
    const generatedAddress = await this.client.getText('.generatedAddress-1 .WalletReceive_addressId');
    const activeAddress = await this.client.getText('.WalletReceive_hash');
    expect(generatedAddress).to.equal(activeAddress);
  });
};
