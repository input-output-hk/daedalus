import { expect } from 'chai';

export default function () {
  this.Given(/^I am on the login screen$/, function () {
    return this.client.waitForVisible('.Login_form');
  });

  this.When(/^I submit login form with the following inputs:$/, async function (table) {
    const fields = table.hashes()[0];
    await this.client.setValue('.Login_form .email input', fields.email);
    await this.client.setValue('.Login_form .password input', fields.password);
    return this.client.click('.Login_form .Login_submitButton');
  });

  this.Given(/^I am logged in$/, async function () {
    await this.client.execute(function () {
      const user = daedalus.api.repository.user;
      const { email, passwordHash } = user.profile;
      daedalus.controller.user.login({ email, passwordHash });
    });
    return this.client.waitForVisible(`.WalletTransactionsSearch_component`, null, true);
  });

  this.Given(/^I dont see the login window(?: anymore)?$/, function () {
    return this.client.waitForVisible('.Login_form', null, true);
  });
}
