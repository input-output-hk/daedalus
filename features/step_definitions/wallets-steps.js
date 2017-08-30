import { expect } from 'chai';
import path from 'path';
import BigNumber from 'bignumber.js';
import {
  fillOutWalletSendForm,
  getWalletByName,
  waitUntilWalletIsLoaded,
  addOrSetWalletsForScenario,
} from '../support/helpers/wallets-helpers';
import {
  waitUntilUrlEquals,
  navigateTo,
} from '../support/helpers/route-helpers'
import { DECIMAL_PLACES_IN_ADA } from '../../app/config/numbersConfig';

const defaultWalletKeyFilePath = path.resolve(__dirname, '../support/default-wallet.key');
const defaultWalletJSONFilePath = path.resolve(__dirname, '../support/default-wallet.json');

export default function () {

  this.Given(/^I have a wallet with funds$/, async function () {
    await this.client.executeAsync(function(filePath, done) {
      daedalus.api.importWalletFromKey({ filePath, walletPassword: null }).then(() => {
        daedalus.stores.wallets.refreshWalletsData().then(done);
      });
    }, defaultWalletKeyFilePath);
    const wallet = await waitUntilWalletIsLoaded.call(this, 'Genesis wallet');
    addOrSetWalletsForScenario.call(this, wallet);
  });

  this.Given(/^I have a wallet with funds and password$/, async function () {
    await this.client.executeAsync(function(filePath, done) {
      daedalus.api.importWalletFromKey({ filePath, walletPassword: 'Secret123' }).then(() => {
        daedalus.stores.wallets.refreshWalletsData().then(done);
      });
    }, defaultWalletKeyFilePath);
    const wallet = await waitUntilWalletIsLoaded.call(this, 'Genesis wallet');
    addOrSetWalletsForScenario.call(this, wallet);
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
        daedalus.stores.wallets.walletsRequest.execute().then((wallets) => {
          daedalus.stores.wallets.refreshWalletsData().then(() => done(wallets));
        });
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
    await navigateTo.call(this, `/wallets/${wallet.id}/${screen}`);
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
    return this.client.waitForVisible('.WalletFileImportDialog');
  });

  this.When(/^I select a valid wallet import key file$/, async function () {
    await this.client.chooseFile('.WalletFileImportDialog .FileUploadWidget_dropZone input', defaultWalletJSONFilePath);
  });

  this.When(/^I toggle "Activate to create password" switch on the import wallet key dialog$/, function () {
    return this.waitAndClick('.WalletFileImportDialog .SimpleSwitch_switch');
  });

  this.When(/^I enter wallet spending password:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletFileImportDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletFileImportDialog .repeatedPassword input', fields.repeatedPassword);
  });

  this.When(/^I click on the import wallet button in import wallet dialog$/, function () {
    return this.waitAndClick('.WalletFileImportDialog .primary');
  });

  this.When(/^I should see wallet spending password inputs$/, function () {
    return this.client.waitForVisible('.WalletFileImportDialog .walletPassword input');
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

  this.When(/^the transaction fees are calculated$/, async function () {
    this.fees = await this.client.waitUntil(async () => {
      // Expected transactionFeeText format "+ 0.000001 of fees"
      const transactionFeeText = await this.client.getText('.AmountInputSkin_fees');
      const transactionFeeAmount = new BigNumber(transactionFeeText.substr(2, 8));
      return transactionFeeAmount.greaterThan(0) ? transactionFeeAmount : false;
    });
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.When(/^I toggle "Activate to create password" switch on the create wallet dialog$/, function () {
    return this.waitAndClick('.WalletCreateDialog .SimpleSwitch_switch');
  });

  this.When(/^I toggle "Activate to create password" switch on the restore wallet dialog$/, function () {
    return this.waitAndClick('.WalletRestoreDialog .SimpleSwitch_switch');
  });

  this.When(/^I submit the create wallet dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
    return this.client.click('.WalletCreateDialog .primary');
  });

  this.When(/^I submit the create wallet with spending password dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletCreateDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletCreateDialog .repeatedPassword input', fields.repeatedPassword);
    return this.client.click('.WalletCreateDialog .primary');
  });

  this.When(/^I submit the restore wallet dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletRestoreDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletRestoreDialog .recoveryPhrase textarea', fields.recoveryPhrase);
    return this.client.click('.WalletRestoreDialog .primary');
  });

  this.When(/^I submit the restore wallet with spending password dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletRestoreDialog .walletName input', fields.walletName);
    await this.client.setValue('.WalletRestoreDialog .recoveryPhrase textarea', fields.recoveryPhrase);
    await this.client.setValue('.WalletRestoreDialog .walletPassword input', fields.password);
    await this.client.setValue('.WalletRestoreDialog .repeatedPassword input', fields.repeatedPassword);
    return this.client.click('.WalletRestoreDialog .primary');
  });

  this.When(/^I see the create wallet privacy dialog$/, function () {
    return this.client.waitForVisible('.WalletBackupPrivacyWarningDialog');
  });

  this.When(/^I click on "Please make sure nobody looks your screen" checkbox$/, function () {
    return this.waitAndClick('.WalletBackupPrivacyWarningDialog .SimpleCheckbox_root');
  });

  this.When(/^I submit the create wallet privacy dialog$/, function () {
    return this.waitAndClick('.WalletBackupPrivacyWarningDialog .primary');
  });

  this.When(/^I see the create wallet recovery phrase display dialog$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseDisplayDialog');
  });

  this.When(/^I note down the recovery phrase$/, async function () {
    const recoveryPhrase = await this.client.getText('.WalletRecoveryPhraseMnemonic_component');
    this.recoveryPhrase = recoveryPhrase.split(' ');
  });

  this.When(/^I submit the create wallet recovery phrase display dialog$/, function () {
    return this.waitAndClick('.WalletRecoveryPhraseDisplayDialog .primary');
  });

  this.When(/^I see the create wallet recovery phrase entry dialog$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog');
  });

  this.When(/^I click on recovery phrase mnemonics in correct order$/, async function () {
    for (let i = 0; i < this.recoveryPhrase.length; i++) {
      const recoveryPhraseMnemonic = this.recoveryPhrase[i];
      await this.waitAndClick(`//button[contains(text(), "${recoveryPhraseMnemonic}") and @class="flat MnemonicWord_component MnemonicWord_active SimpleButton_root"]`);
    }
  });

  this.When(/^I click on the "Accept terms" checkboxes$/, async function () {
    const termsCheckboxes = await this.client.elements('.SimpleCheckbox_root');
    for (let i = 0; i < termsCheckboxes.value.length; i++) {
      const termsCheckbox = termsCheckboxes.value[i].ELEMENT;
      await this.client.elementIdClick(termsCheckbox);
    }
  });

  this.When(/^I submit the create wallet recovery phrase entry dialog$/, function () {
    return this.waitAndClick('.WalletRecoveryPhraseEntryDialog .primary');
  });

  this.When(/^I click on delete wallet button$/, async function () {
    return this.client.click('.DeleteWalletButton_button');
  });

  this.When(/^I enter "([^"]*)" as name of the wallet to confirm$/, async function (walletName) {
    return this.client.setValue('.DeleteWalletConfirmationDialog_confirmationInput input', walletName);
  });

  this.When(/^I click on the "Make sure you have access to backup before continuing" checkbox$/, function () {
    return this.waitAndClick('.DeleteWalletConfirmationDialog_dialog .SimpleCheckbox_root');
  });

  this.When(/^I submit the delete wallet dialog$/, async function () {
    return this.client.click('.DeleteWalletConfirmationDialog_dialog .primary');
  });

  this.Then(/^I should not see the create wallet recovery phrase entry dialog anymore$/, function () {
    return this.client.waitForVisible('.WalletRecoveryPhraseEntryDialog', null, true);
  });

  this.Then(/^I should not see the delete wallet dialog anymore$/, function () {
    return this.client.waitForVisible('.DeleteWalletConfirmationDialog_dialog', null, true);
  });

  this.Then(/^I should not see the import wallet dialog anymore$/, function () {
    return this.client.waitForVisible('.WalletFileImportDialog', null, true);
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
    // Transaction amount includes transaction fees so we need to
    // substract them in order to get a match with expectedData.amountWithoutFees.
    // NOTE: we use "add()" as this is outgoing transaction and amount is a negative value!
    const transactionAmount = new BigNumber(transactionAmounts[0]);
    const transactionAmountWithoutFees = transactionAmount.add(this.fees).toFormat(DECIMAL_PLACES_IN_ADA);
    expect(expectedData.amountWithoutFees).to.equal(transactionAmountWithoutFees);
  });

  this.Then(/^the balance of "([^"]*)" wallet should be:$/, async function (walletName, table) {
    const expectedData = table.hashes()[0];
    const receiverWallet = getWalletByName.call(this, walletName);
    return this.client.waitUntil(async () => {
      const receiverWalletBalance = await this.client.getText(`.SidebarWalletsMenu_wallets .Wallet_${receiverWallet.id} .SidebarWalletMenuItem_info`);
      return receiverWalletBalance === `${expectedData.balance} ADA`;
    });
  });

  this.Then(/^I should see newly generated address as active address on the wallet receive screen$/, async function () {
    return this.client.waitUntil(async () => {
      const activeAddress = await this.client.getText('.WalletReceive_hash');
      const generatedAddress = await this.client.getText('.generatedAddress-1 .WalletReceive_addressId');
      return generatedAddress === activeAddress;
    });
  });
};
