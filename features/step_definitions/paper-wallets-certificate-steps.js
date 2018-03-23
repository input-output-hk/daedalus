import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import path from 'path';

const paperWalletCertificatePath = path.resolve(__dirname, '../support/paper_wallet_certificates/paper-wallet-certificate.pdf');

Given(/^I see the "Certificate Generation Instructions" dialog$/, function () {
  return this.client.waitForVisible('.instructionsDialog');
});

When(/^I click on the continue button$/, function () {
  return this.waitAndClick('.continueButton');
});

When(/^I see the "Password Choice" dialog$/, function () {
  return this.client.waitForVisible('.passwordChoiceDialog');
});

When(/^I enter shielded password:$/, async function (table) {
  const fields = table.hashes()[0];
  await this.client.setValue('.PasswordChoiceDialog_passwordWrapper input', fields.password);
  await this.client.setValue('.PasswordChoiceDialog_repeatPasswordWrapper input', fields.repeatedPassword);
});

When(/^I click on "I understand the importance of the password and I will keep it secure." checkbox$/, function () {
  return this.waitAndClick('.passwordChoiceDialog .SimpleCheckbox_root');
});

When(/^I click on the print button$/, async function () {
  const password = await this.client.getValue('.PasswordChoiceDialog_passwordWrapper input');
  const repeatedPassword = await this.client.getValue('.PasswordChoiceDialog_repeatPasswordWrapper input');

  const data = {
    password,
    repeatedPassword,
    filePath: paperWalletCertificatePath,
  };

  await this.client.execute(params => {
    daedalus.actions.ada.wallets.generateCertificate.trigger(params);
  }, data);
  // set entered password in scope
  this.walletCertificatePassword = password;
});

When(/^I see the "Certificate Generation Complete" dialog$/, function () {
  return this.client.waitForVisible('.printDialog');
});

When(/^I click on "Yes, paper wallet certificate successfully printed and everything is readable and scannable." checkbox$/, function () {
  return this.waitAndClick('.printDialog .SimpleCheckbox_root');
});

When(/^Continue button is no longer disabled$/, function () {
  return this.client.waitForEnabled('.continueButton');
});

When(/^Print button is no longer disabled$/, function () {
  return this.client.waitForEnabled('.printButton');
});

When(/^I see the "Certificate Password" dialog$/, function () {
  return this.client.waitForVisible('.SecuringPasswordDialog');
});

When(/^Shown password should be equal to entered password$/, async function () {
  const visiblePassword = await this.client.getText('.SecuringPasswordDialog_recoveryPhrase');
  expect(visiblePassword).to.equal(this.walletCertificatePassword);
});

When(/^I click on "I understand that I can not use my certificate without the password and I have stored it safely." checkbox$/, function () {
  return this.waitAndClick('.SecuringPasswordDialog .SimpleCheckbox_root');
});

When(/^I see the "Verify Certificate" dialog$/, function () {
  return this.client.waitForVisible('.verificationDialog');
});

When(/^I enter shielded recovery phrase$/, async function () {
  const fields = await this.client.execute(() => (
    daedalus.stores.ada.wallets.walletCertificateRecoveryPhrase
  ));

  if (!this.walletCertificateRecoveryPhrase) {
    this.walletCertificateRecoveryPhrase = fields.value;
  }

  const recoveryPhrase = this.walletCertificateRecoveryPhrase.split(' ');
  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.SimpleAutocomplete_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[contains(text(), '${word}')]`);
    await this.waitAndClick(`//li[contains(text(), '${word}')]`);
    await this.client.waitForVisible(`//span[contains(text(), '${word}')]`);
  }
});

When(/^I enter wrong shielded recovery phrase:$/, async function (table) {
  const fields = table.hashes()[0];
  const recoveryPhrase = fields.recoveryPhrase.split(' ');
  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.SimpleAutocomplete_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[contains(text(), '${word}')]`);
    await this.waitAndClick(`//li[contains(text(), '${word}')]`);
    await this.client.waitForVisible(`//span[contains(text(), '${word}')]`);
  }
});

When(/^I should see the following field error message:$/, async function (data) {
  const error = data.hashes()[0];
  const errorSelector = '.VerificationDialog_recoveryPhrase .SimpleFormField_error';
  await this.client.waitForText(errorSelector);
  const errorsOnScreen = await this.client.getText(errorSelector);
  const expectedError = await this.intl(error.message);
  expect(errorsOnScreen).to.equal(expectedError);
});

When(/^Checkboxes should be disabled$/, async function () {
  const chk1 = !await this.client.isEnabled('.storingUnderstandance input');
  const chk2 = !await this.client.isEnabled('.recoveringUnderstandance input');
  expect(chk1 && chk2).to.equal(true);
});

When(/^Continue button should be disabled$/, async function () {
  const isEnabled = await this.client.isEnabled('.continueButton');
  expect(isEnabled).to.equal(false);
});

When(/^I enter password "([^"]*)"$/, function (password) {
  return this.client.setValue('.VerificationDialog_password input', password);
});

When(/^Checkboxes are no longer disabled$/, async function () {
  await this.client.waitForEnabled('.storingUnderstandance input');
  await this.client.waitForEnabled('.recoveringUnderstandance input');
});

When(/^I click on "I understand that the created wallet will not be stored in Daedalus after this step." checkbox$/, function () {
  return this.waitAndClick('.storingUnderstandance');
});

When(/^I click on "I understand that my wallet can only be recovered using my paper wallet certificate and the password I have chosen." checkbox$/, function () {
  return this.waitAndClick('.recoveringUnderstandance');
});

When(/^I see the "Paper Wallet Certificate" dialog$/, function () {
  return this.client.waitForVisible('.completionDialog');
});

When(/^Cardano explorer link should be valid$/, async function () {
  const visibleAddress = await this.client.getText('.CompletionDialog_cardanoExplorerLinkWrapper a');
  const walletCertificateAddress = await this.client.execute(() => (
    daedalus.stores.ada.wallets.walletCertificateAddress
  ));
  const providedAddress = `https://cardanoexplorer.com/address/${walletCertificateAddress.value}`;
  this.certificateWalletAddress = walletCertificateAddress.value;

  expect(providedAddress).to.equal(visibleAddress);
});

When(/^I click on the finish button$/, function () {
  return this.waitAndClick('.finishButton');
});

When(/^I should not see the create paper wallet certificate dialog anymore$/, function () {
  return this.client.waitForVisible('.completionDialog', null, true);
});

When(/^I click "Paper wallet certificate" tab$/, function () {
  return this.waitAndClick('.certificateTab');
});

When(/^I see "Restore wallet with certificate" form$/, function () {
  return this.client.waitForVisible('.WalletRestoreDialog_dialogWithCertificateRestore');
});

When(/^I enter paper wallet certificate password$/, function () {
  return this.client.setValue('.WalletRestoreDialog_certificatePassword input', this.walletCertificatePassword);
});

When(/^I send money to paper wallet certificate dialog$/, async function () {
  const data = {
    amount: 20,
    password: null,
    receiver: this.certificateWalletAddress,
  };

  await this.client.execute(params => {
    daedalus.actions.ada.wallets.sendMoney.trigger(params);
  }, data);
});

Then(/^I should see that address was used$/, async function () {
  const addressSelector = '.WalletReceive_usedWalletAddress .WalletReceive_addressId';
  await this.client.waitForVisible(addressSelector);
  const usedAddress = await this.client.getText(addressSelector);
  expect(usedAddress).to.equal(this.certificateWalletAddress);
});
