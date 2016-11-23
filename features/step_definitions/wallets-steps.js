import { expect } from 'chai';

export default function () {

  this.Given(/^I have the following wallets:$/, async function (table) {
    const result = await this.client.execute(function() {
      const browserRequire = require;
      return browserRequire('./api/data/wallets.json')
    });
    console.log(result);
  });

  this.Given(/^I am on the wallet send screen$/, async function() {
    this.client.url('/wallet/')
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_sendButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.Then(/^I should see the following error messages:$/, async function (data) {
    const errorsOnScreen = await this.client.getText('.WalletSendForm_textField > :last-child');
    const errors = data.hashes();
    for (let i=0; i < errors.length; i++) {
      expect(errorsOnScreen[i]).to.equal(errors[i].message);
    }
  });

};
