import { expect } from 'chai';
import {
  waitUntilWaletNamesEqual,
  getNameOfActiveWalletInSidebar
} from './lib/wallets-helpers';

export default function () {

  this.Given(/^I should see the "([^"]*)" wallet password dialog$/, function (dialogType) {
    const selector = '.' + dialogType + 'PasswordDialog';
    return this.client.waitForVisible(selector);
  });

  this.When(/^I click on the "([^"]*)" password label$/, function (label) {
    const selector = '.' + label + 'Label button';
    return this.client.click(selector);
  });

  this.When(/^I enter wallet password:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.createPasswordDialog .newPassword input', fields.password);
    await this.client.setValue('.createPasswordDialog .repeatedPassword input', fields.repeatedPassword);
  });

  this.When(/^I click on the "([^"]*)" button in "([^"]*)" wallet password dialog$/, function (action, dialogType) {
    return this.client.click('.confirmButton');
  });

  this.When(/^I change wallet password:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.changePasswordDialog .currentPassword input', fields.currentPassword);
    await this.client.setValue('.changePasswordDialog .newPassword input', fields.password);
    await this.client.setValue('.changePasswordDialog .repeatedPassword input', fields.repeatedPassword);
  });

  this.Then(/^I should not see the change password dialog anymore$/, function () {
    return this.client.waitForVisible('.changePasswordDialog', null, true);
  });

  this.When(/^I toggle "Check to deactivate password" switch on the change wallet password dialog$/, function () {
    return this.waitAndClick('.changePasswordDialog .switch_field');
  });

  this.When(/^I enter current wallet password:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.changePasswordDialog .currentPassword input', fields.currentPassword);
  });

  this.When(/^I click on "name" input field$/, function () {
    return this.client.click('.WalletSettings_component .InlineEditingInput_component');
  });

  this.When(/^I enter new wallet name:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.WalletSettings_component .InlineEditingInput_component input', fields.name);
  });

  this.When(/^I click outside "name" input field$/, function () {
    return this.client.click('.WalletSettings_component');
  });

  this.When(/^I open "Transaction assurance security level" selection dropdown$/, function () {
    return this.waitAndClick('.WalletSettings_assuranceLevelSelect .SimpleInput_input');
  });

  this.When(/^I select "Strict" assurance level$/, function () {
    return this.waitAndClick('//li[contains(text(), "Strict")]');
  });

  this.Then(/^I should have wallet with "Strict" assurance level set$/, async function () {
    const activeWalletName = await getNameOfActiveWalletInSidebar.call(this);
    const wallets = await this.client.executeAsync(function(done) {
      daedalus.stores.wallets.walletsRequest.execute().then(done);
    });
    const activeWallet = wallets.value.find((w) => w.name === activeWalletName);
    expect(activeWallet.assurance).to.equal('CWAStrict');
  });

  this.Then(/^I should see new wallet name "([^"]*)"$/, async function (walletName) {
    return waitUntilWaletNamesEqual.call(this, walletName);
  });

  this.Then(/^I should see "([^"]*)" label in password field$/, function (label) {
    const selector = '.' + label + 'Label';
    return this.client.waitForVisible(selector);
  });

  this.Then(/^I should see the following error messages:$/, async function (data) {
    const error = data.hashes()[0];
    const errorSelector = '.ChangeWalletPasswordDialog_newPassword .input_error';
    await this.client.waitForText(errorSelector);
    let errorsOnScreen = await this.client.getText(errorSelector);
    const expectedError = await this.intl(error.message);
    expect(errorsOnScreen).to.equal(expectedError);
  });
}
