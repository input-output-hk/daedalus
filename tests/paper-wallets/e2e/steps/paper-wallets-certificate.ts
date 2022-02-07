import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import path from "path";
import { fillOutWalletSendForm } from "../../../wallets/e2e/steps/helpers";
import { waitUntilTextInSelector } from "../../../common/e2e/steps/helpers";

const paperWalletCertificatePath = path.resolve(__dirname, '../documents/paper-wallet-certificate.pdf');
Given(/^I see the "Certificate Generation Instructions" dialog$/, function () {
  return this.client.waitForVisible('.instructionsDialog');
});
When(/^I click on the print button$/, async function () {
  /**
   * Clicking the real button would open the system dialog which we cannot test
   * easily. So we just skip that step and pretend the user picked a path
   */
  const data = {
    filePath: paperWalletCertificatePath
  };
  await this.client.execute(params => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.wallets.generateCertificate.trigger({
      filePath: params.filePath
    });
  }, data);
});
When(/^I click on the continue button$/, function () {
  return this.waitAndClick('.continueButton');
});
When(/^I see the "Certificate Generation Complete" dialog$/, function () {
  return this.client.waitForVisible('.printDialog');
});
When(/^I check all "Print Dialog" checkboxes$/, async function () {
  await this.waitAndClick('.printDialog .printedCheckbox');
  await this.waitAndClick('.printDialog .readableCheckbox');
  await this.waitAndClick('.printDialog .scannableCheckbox');
});
When(/^I see the "Securing Additional mnemonics" dialog$/, function () {
  return this.client.waitForVisible('.SecuringPasswordDialog');
});
When(/^I click on "I have written the remaining 9 words to the certificate." checkbox$/, function () {
  return this.waitAndClick('.SecuringPasswordDialog .SimpleCheckbox_root');
});
When(/^I see the "Verify Certificate" dialog$/, function () {
  return this.client.waitForVisible('.verificationDialog');
});
When(/^I enter paper wallet recovery phrase$/, async function () {
  const fields = await this.client.execute(() => ({
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    walletCertificateRecoveryPhrase: daedalus.stores.wallets.walletCertificateRecoveryPhrase,
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    additionalMnemonicWords: daedalus.stores.wallets.additionalMnemonicWords
  }));
  const walletCertificateRecoveryPhrase = `${fields.value.walletCertificateRecoveryPhrase} ${fields.value.additionalMnemonicWords}`;

  if (!this.walletCertificateRecoveryPhrase) {
    this.walletCertificateRecoveryPhrase = walletCertificateRecoveryPhrase;
  }

  const recoveryPhrase = this.walletCertificateRecoveryPhrase.split(' ');

  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.AutocompleteOverrides_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});
When(/^I enter wrong paper wallet recovery phrase:$/, async function (table) {
  const fields = table.hashes()[0];
  const recoveryPhrase = fields.recoveryPhrase.split(' ');

  for (let i = 0; i < recoveryPhrase.length; i++) {
    const word = recoveryPhrase[i];
    await this.client.setValue('.AutocompleteOverrides_autocompleteWrapper input', word);
    await this.client.waitForVisible(`//li[text()="${word}"]`);
    await this.waitAndClick(`//li[text()="${word}"]`);
    await this.client.waitForVisible(`//span[text()="${word}"]`);
  }
});
When(/^I fill out the send form:$/, async function (table) {
  const values = table.hashes()[0];
  values.address = this.certificateWalletAddress;
  return fillOutWalletSendForm.call(this, values);
});
When(/^I should see the following field error message:$/, async function (data) {
  const error = data.hashes()[0];
  await waitUntilTextInSelector(this.client, {
    selector: '.VerificationDialog_recoveryPhrase .SimpleFormField_error',
    text: await this.intl(error.message)
  });
});
When(/^Verify certificate checkboxes should be disabled$/, async function () {
  const chk1 = !(await this.client.isEnabled('.storingUnderstandance input'));
  const chk2 = !(await this.client.isEnabled('.recoveringUnderstandance input'));
  expect(chk1 && chk2).to.equal(true);
});
When(/^Continue button should be disabled$/, async function () {
  const isEnabled = await this.client.isEnabled('.continueButton');
  expect(isEnabled).to.equal(false);
});
When(/^Verify certificate checkboxes are no longer disabled$/, async function () {
  await this.client.waitForEnabled('.storingUnderstandance input');
  await this.client.waitForEnabled('.recoveringUnderstandance input');
});
When(/^I check all "Verify Certificate" checkboxes$/, async function () {
  await this.waitAndClick('.storingUnderstandance');
  await this.waitAndClick('.recoveringUnderstandance');
});
When(/^I see the "Paper Wallet Certificate" dialog$/, function () {
  return this.client.waitForVisible('.completionDialog');
});
When(/^Cardano explorer link and wallet address should be valid$/, async function () {
  const visibleCardanoExplorerLink = await this.waitAndGetText('.CompletionDialog_linkInstructionsWrapper .CompletionDialog_infoBox .CompletionDialog_link');
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  const walletCertificateAddress = await this.client.execute(() => daedalus.stores.wallets.walletCertificateAddress);
  const cardanoExplorerLink = `https://explorer.cardano.org/en/address${walletCertificateAddress.value}`;
  this.certificateWalletAddress = walletCertificateAddress.value;
  const visibleWalletAddress = await this.waitAndGetText('.CompletionDialog_addressInstructionsWrapper .CompletionDialog_infoBox');
  expect(cardanoExplorerLink).to.equal(visibleCardanoExplorerLink);
  expect(walletCertificateAddress.value).to.equal(visibleWalletAddress);
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
Then(/^I should see that address was used$/, async function () {
  const addressSelector = '.Address_usedWalletAddress .Address_addressId';
  const usedAddress = await this.waitAndGetText(addressSelector);
  expect(usedAddress).to.equal(this.certificateWalletAddress);
});