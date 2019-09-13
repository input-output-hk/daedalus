import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { navigateTo } from '../helpers/route-helpers';
import {
  waitUntilWaletNamesEqual,
  getNameOfActiveWalletInSidebar,
} from '../helpers/wallets-helpers';

Given(/^I am on the settings screen$/, async function() {
  await navigateTo.call(this, '/settings');
  return this.client.waitForVisible('.SettingsLayout_component');
});

Given(/^I should see the "([^"]*)" wallet password dialog$/, function(
  dialogType
) {
  const selector = `.${dialogType}PasswordDialog`;
  return this.client.waitForVisible(selector);
});

When(/^I click on the "([^"]*)" password label$/, function(label) {
  const selector = `.${label}Label button`;
  return this.client.click(selector);
});

When(/^I enter wallet password:$/, async function(table) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.createPasswordDialog .newPassword input',
    fields.password
  );
  await this.client.setValue(
    '.createPasswordDialog .repeatedPassword input',
    fields.repeatedPassword
  );
});

When(/^I submit the wallet password dialog$/, function() {
  return this.client.click('.confirmButton');
});

When(/^I change wallet password:$/, async function(table) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.changePasswordDialog .currentPassword input',
    fields.currentPassword
  );
  await this.client.setValue(
    '.changePasswordDialog .newPassword input',
    fields.password
  );
  await this.client.setValue(
    '.changePasswordDialog .repeatedPassword input',
    fields.repeatedPassword
  );
});

Then(/^I should not see the change password dialog anymore$/, function() {
  return this.client.waitForVisible('.changePasswordDialog', null, true);
});

When(
  /^I toggle "Check to deactivate password" switch on the change wallet password dialog$/,
  function() {
    return this.waitAndClick('.changePasswordDialog .SimpleSwitch_switch');
  }
);

When(/^I enter current wallet password:$/, async function(table) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.changePasswordDialog .currentPassword input',
    fields.currentPassword
  );
});

When(/^I click on "name" input field$/, function() {
  return this.client.click(
    '.WalletSettings_component .InlineEditingInput_component'
  );
});

When(/^I enter new wallet name:$/, async function(table) {
  const fields = table.hashes()[0];
  await this.client.setValue(
    '.WalletSettings_component .walletName input',
    fields.name
  );
});

When(/^I click outside "name" input field$/, function() {
  return this.client.click('.WalletSettings_component');
});

When(
  /^I open "Transaction assurance security level" selection dropdown$/,
  function() {
    return this.waitAndClick(
      '.WalletSettings_component .walletAssuranceLevel input'
    );
  }
);

When(/^I select "Strict" assurance level$/, function() {
  return this.waitAndClick(
    '//*[@class="SimpleOptions_option"]//*[contains(text(), "Strict")]'
  );
});

Then(
  /^I should have wallet with "Strict" assurance level set$/,
  async function() {
    const activeWalletName = await getNameOfActiveWalletInSidebar.call(this);
    const wallets = await this.client.executeAsync(done => {
      daedalus.stores.wallets.walletsRequest
        .execute()
        .then(done)
        .catch(error => done(error));
    });
    const activeWallet = wallets.value.find(w => w.name === activeWalletName);
    expect(activeWallet.assurance).to.equal('strict');
  }
);

Then(/^I should see new wallet name "([^"]*)"$/, async function(walletName) {
  return waitUntilWaletNamesEqual.call(this, walletName);
});

Then(/^I should see "([^"]*)" label in password field$/, function(label) {
  const selector = `.${label}Label`;
  return this.client.waitForVisible(selector);
});

Then(/^I should see the following error messages:$/, async function(data) {
  const error = data.hashes()[0];
  const errorSelector =
    '.ChangeSpendingPasswordDialog_newPassword .SimpleFormField_error';
  await this.client.waitForText(errorSelector);
  const errorsOnScreen = await this.client.getText(errorSelector);
  const expectedError = await this.intl(error.message);
  expect(errorsOnScreen).to.equal(expectedError);
});
