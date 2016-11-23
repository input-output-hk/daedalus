import { expect } from 'chai';

export default function () {

  this.Given(/^I have a wallet$/, async function () {
    const result = await this.client.execute(function() {
      const wallet = daedalus.api.data.createWallet();
      daedalus.controller.wallets.loadWallets();
      return wallet;
    });
    this.wallet = result.value;
  });

  this.Given(/^I am on the wallet send screen$/, async function() {
    await this.client.waitForVisible('.WalletNavigation_sendLink');
    return this.client.execute(function(route) {
      daedalus.state.router.transitionTo(route);
    }, `/wallet/${this.wallet.address}/send`);
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.Then(/^I should see the following error messages on the wallet send form:$/, async function (data) {
    const errorsOnScreen = await this.client.getText('.WalletSendForm_fields .input_error');
    const errors = data.hashes();
    for (let i=0; i < errors.length; i++) {
      expect(errorsOnScreen[i]).to.equal(errors[i].message);
    }
  });

};
