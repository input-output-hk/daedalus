import { expect } from 'chai';
import {
  fillOutWalletSendForm,
  expectActiveWallet,
  getNameOfActiveWalletInSidebar
} from './lib/wallets-helpers';
import path from 'path';

const defaultWalletKeyFilePath = path.resolve(__dirname, '../support/default-wallet.key');

export default function () {

  this.Given(/^I have a wallet$/, async function () {
    const result = await this.client.executeAsync(function(filePath, done) {
      // This assumes that we always have a default wallet on the backend!
      daedalus.api.importWalletFromKey({ filePath }).then((wallet) => done(wallet));
    }, defaultWalletKeyFilePath);
    this.wallet = result.value;
    this.wallets = [this.wallet];
  });

  this.Given(/^I have the following wallets:$/, async function (table) {
    const result = await this.client.executeAsync(function(wallets, done) {
      window.Promise.all(wallets.map((wallet) => {
        return daedalus.api.createWallet({
          name: wallet.name,
          mnemonic: daedalus.api.generateMnemonic().join(' ')
        });
      }))
      .then(() => {
        daedalus.stores.wallets.walletsRequest.invalidate({ immediately: true }).then(done);
      })
      .catch((error) => done(error.stack));
    }, table.hashes());
    console.log(result.value);
    this.wallets = result.value;
  });

  this.Given(/^I am on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screen) {
    const wallet = this.wallets.find((w) => w.name === walletName);
    await this.navigateTo(`/wallets/${wallet.id}/${screen}`);
    return expectActiveWallet.call(this, walletName);
  });

  this.Given(/^I see the create wallet dialog$/, function () {
    return this.client.waitForVisible('.WalletCreateDialog');
  });

  this.Given(/^I dont see the create wallet dialog(?: anymore)?$/, function () {
    return this.client.waitForVisible('.WalletCreateDialog', null, true);
  });

  this.When(/^I click on the (.*) wallet in the sidebar$/, function (walletName) {
    return this.client.click(`//*[contains(text(), "${walletName}") and @class="SidebarWalletMenuItem_title"]`);
  });

  this.When(/^I click the wallet (.*) button$/, async function (buttonName) {
    const buttonSelector = `.WalletNavButton_component.${buttonName}`;
    await this.client.waitForVisible(buttonSelector);
    await this.client.click(buttonSelector);
  });

  this.When(/^I fill out the wallet send form with:$/, function (table) {
    return fillOutWalletSendForm.call(this, table.hashes()[0]);
  });

  this.When(/^I fill out the send form with a transaction to "([^"]*)" wallet:$/, function (walletName, table) {
    const values = table.hashes()[0];
    values.address = this.wallets.find((w) => w.name === walletName).address;
    return fillOutWalletSendForm.call(this, values);
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.When(/^I submit the create wallet dialog with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
    return this.client.click('.WalletCreateDialog .dialog_button');
  });

  this.Then(/^I should be on some wallet page$/, async function () {
    return this.client.waitForVisible('.WalletNavigation_component');
  });

  // TODO: Refactor this to include previous step definition
  this.Then(/^I should be on the(.*)? wallet (.*) screen$/, async function (walletName, screenName) {
    if (walletName) await expectActiveWallet.call(this, walletName);
    return this.client.waitForVisible(`.WalletNavButton_active.${screenName}`);
  });

  this.Then(/^I should see the following error messages on the wallet send form:$/, async function (data) {
    let errorsOnScreen = await this.client.getText('.WalletSendForm_fields .input_error');
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i=0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  });

  this.Then(/^I should see the "([^"]*)" wallet home screen with the transaction titled "([^"]*)"$/, async function (walletName, title) {
    const wallet = this.wallets.find((w) => w.name === walletName);
    const displayedWalletName = await getNameOfActiveWalletInSidebar.call(this);
    expect(displayedWalletName.toLowerCase()).to.equal(wallet.name.toLowerCase());
    const transactionTitle = await this.client.getText('.Transaction_title');
    expect(transactionTitle).to.equal(title);
    const transactionType = await this.client.getText('.Transaction_type');
    expect(transactionType).to.equal('ADA transaction');
    const transactionAmount = await this.client.getText('.Transaction_amount');
    expect(transactionAmount).to.equal(`-${this.walletSendFormValues['amount']} ada`);
  });

};
