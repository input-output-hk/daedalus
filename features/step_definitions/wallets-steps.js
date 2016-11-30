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

  this.Given(/^I am on the wallet (.*) screen$/, async function(screen) {
    await this.navigateTo(`/wallet/${this.wallet.address}/${screen}`);
    await this.client.waitForVisible('.WalletWithNavigation_component');
  });

  this.When(/^I click the wallet (.*) button$/, async function (buttonName) {
    const buttonSelector = `.WalletNavigation_${buttonName}Link`;
    await this.client.waitForVisible(buttonSelector);
    await this.client.click(buttonSelector);
  });

  this.When(/^I fill out the wallet send form with:$/, async function (table) {
    const values = table.hashes()[0];
    const formSelector = '.WalletSendForm_fields';
    await this.client.setValue(`${formSelector} .receiver .input_inputElement`, values.receiver);
    await this.client.setValue(`${formSelector} .amount .input_inputElement`, values.amount);
    await this.client.setValue(`${formSelector} .description .input_inputElement`, values.description);
    this.walletSendFormValues = values;
  });

  this.When(/^I submit the wallet send form$/, async function () {
    const submitButton = '.WalletSendForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });

  this.Then(/^I should be on the wallet (.*) screen$/, async function (screen) {
    if (screen != 'home') {
      const buttonSelector = `.WalletNavigation_${screen}Link`;
      await this.client.waitForVisible(`${buttonSelector} .WalletNavButton_active`);
    } else {
      await this.client.waitForVisible('.WalletHomeButton_active');
    }
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

  this.Then(/^I should see the wallet home screen with the transaction$/, async function () {
    await this.client.waitForVisible('.WalletHomeButton_active');
    const visibleWalletName = await this.client.getText('.WalletHomeButton_walletName');
    expect(visibleWalletName.toLowerCase()).to.equal(this.wallet.name.toLowerCase());
    const transactionTitle = await this.client.getText('.Transaction_title');
    expect(transactionTitle).to.equal(`Money to ${this.walletSendFormValues['receiver']}`);
    const transactionType = await this.client.getText('.Transaction_type');
    expect(transactionType).to.equal('ADA transaction');
    const transactionAmount = await this.client.getText('.Transaction_amount');
    expect(transactionAmount).to.equal(`-${this.walletSendFormValues['amount']} ada`);
  });

};
