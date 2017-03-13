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
      daedalus.api.importWalletFromKey({ filePath }).then((wallet) => {
        daedalus.stores.wallets.refreshWalletsData().then(() => done(wallet))
      });
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

  this.Then(/^I should be on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screenName) {
    const wallet = getWalletByName.call(this, walletName);
    return waitUntilUrlEquals.call(this, `/wallets/${wallet.id}/${screenName}`);
  });

  this.Then(/^I should see the following error messages on the wallet send form:$/, async function (data) {
    await this.client.waitForText('.WalletSendForm_fields .input_error');
    let errorsOnScreen = await this.client.getText('.WalletSendForm_fields .input_error');
    if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
    const errors = data.hashes();
    for (let i=0; i < errors.length; i++) {
      const expectedError = await this.intl(errors[i].message);
      expect(errorsOnScreen[i]).to.equal(expectedError);
    }
  });

  this.Then(/^the latest transaction should show:$/, async function (table) {
    const expectedData = table.hashes()[0];
    await this.client.waitForVisible('.Transaction_title');
    const transactionTitle = await this.client.getText('.Transaction_title');
    expect(transactionTitle[0]).to.equal(expectedData.title);
    const transactionAmount = await this.client.getText('.Transaction_amount');
    expect(transactionAmount[0]).to.include(expectedData.amount);
  });

};
