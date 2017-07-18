import path from 'path';
import { navigateTo } from '../support/helpers/route-helpers';

const regularAdaCertificateFilePath = path.resolve(__dirname, '../support/ada_certificates/regular.pdf');
const regularEncryptedAdaCertificateFilePath = path.resolve(__dirname, '../support/ada_certificates/regular.pdf.enc');
const forceVendedAdaCertificateFilePath = path.resolve(__dirname, '../support/ada_certificates/force-vended.pdf');
const forceVendedEncryptedAdaCertificateFilePath = path.resolve(__dirname, '../support/ada_certificates/force-vended.pdf.enc');

export default function () {
  this.Given(/^I have accepted "Daedalus Redemption Disclaimer"$/, async function () {
    await this.client.execute(() => {
      daedalus.actions.adaRedemption.acceptRedemptionDisclaimer.trigger();
    });
  });

  this.Given(/^I am on the ada redemption screen$/, async function () {
    await navigateTo.call(this, '/ada-redemption');
    return this.client.waitForVisible(`.AdaRedemptionForm_component`);
  });

  this.Given(/^I see the "Daedalus Redemption Disclaimer" overlay$/, function () {
    return this.client.waitForVisible('.AdaRedemptionDisclaimer_component');
  });

  this.When(/^I click on the "I\'ve understood the information above" checkbox$/, function () {
    return this.waitAndClick('.AdaRedemptionDisclaimer_component .CheckboxWithLongLabel_checkbox');
  });

  this.When(/^I click on the "Continue" button$/, function () {
    return this.waitAndClick('.AdaRedemptionDisclaimer_component button');
  });

  this.Then(/^I should not see the "Daedalus Redemption Disclaimer" overlay anymore$/, function () {
    return this.client.waitForVisible('.AdaRedemptionDisclaimer_component', null, true);
  });

  this.Then(/^I should(?: still)? be on the ada redemption screen$/, function () {
    return this.client.waitForVisible(`.AdaRedemptionForm_component`);
  });

  this.When(/^I click on ada redemption choices "([^"]*)" tab$/, function (tabText) {
    return this.waitAndClick(`//div[@class="AdaRedemptionChoices_component"]/button[contains(text(), "${tabText}")]`);
  });

  this.When(/^I enter a valid "Regular" redemption key$/, function () {
    const redemptionKey = 'llVRYvW7LAyqmDMnUOvrs5ih4OHfLiLZrz5NT+iRuTw=';
    return this.client.setValue('.AdaRedemptionForm_component .redemption-key input', redemptionKey);
  });

  this.When(/^I select a valid "Regular" PDF certificate$/, async function () {
    await this.client.chooseFile('.AdaRedemptionForm_certificate .AdaCertificateUploadWidget_uploadBox input', regularAdaCertificateFilePath);
  });

  this.When(/^I select a valid "Regular" encrypted PDF certificate$/, async function () {
    await this.client.chooseFile('.AdaRedemptionForm_certificate .AdaCertificateUploadWidget_uploadBox input', regularEncryptedAdaCertificateFilePath);
  });

  this.When(/^I enter a valid "Regular" encrypted PDF certificate passphrase$/, function () {
    const passphrase = 'uncle bargain pistol obtain amount laugh explain type learn';
    return this.client.setValue('.AdaRedemptionForm_component .pass-phrase input', passphrase);
  });

  this.When(/^I enter a valid "Force vended" redemption key$/, function () {
    const redemptionKey = 'LtOD4vxIqfEUYheTiHprRmvmAXHvMJbulllqHhjAGHc=';
    return this.client.setValue('.AdaRedemptionForm_component .redemption-key input', redemptionKey);
  });

  this.When(/^I select a valid "Force vended" PDF certificate$/, async function () {
    await this.client.chooseFile('.AdaRedemptionForm_certificate .AdaCertificateUploadWidget_uploadBox input', forceVendedAdaCertificateFilePath);
  });

  this.When(/^I select a valid "Force vended" encrypted PDF certificate$/, async function () {
    await this.client.chooseFile('.AdaRedemptionForm_certificate .AdaCertificateUploadWidget_uploadBox input', forceVendedEncryptedAdaCertificateFilePath);
  });

  this.When(/^I enter a valid "Force vended" encrypted PDF certificate email, passcode and amount$/, async function () {
    const email = 'nnmbsds@example.org';
    const passcode = 'uilfeet';
    const amount = '12345';
    await this.client.setValue('.AdaRedemptionForm_component .email input', email);
    await this.client.setValue('.AdaRedemptionForm_component .ada-passcode input', passcode);
    await this.client.setValue('.AdaRedemptionForm_component .ada-amount input', amount);
  });

  this.When(/^I enter a valid "Paper vended" shielded vending key$/, function () {
    return this.client.setValue('.AdaRedemptionForm_component .shielded-redemption-key input', '6ANn43jbzR7zZGnV3BYnna1myW5HajPgjiCPg4vpcayf');
  });

  this.When(/^I enter a valid "Paper vended" shielded vending key passphrase$/, function () {
    const passphrase = 'fitness engage danger escape marriage answer coffee develop afraid';
    return this.client.setValue('.AdaRedemptionForm_component .pass-phrase input', passphrase);
  });

  this.When(/^ada redemption form submit button is no longer disabled$/, function () {
    return this.client.waitForEnabled('.AdaRedemptionForm_component .AdaRedemptionForm_submitButton');
  });

  this.When(/^I submit the ada redemption form$/, function () {
    return this.waitAndClick('.AdaRedemptionForm_component .AdaRedemptionForm_submitButton');
  });

  this.Then(/^I should see the "Ada Redemption Success Overlay"$/, function () {
    return this.client.waitForVisible(`.AdaRedemptionSuccessOverlay_component`);
  });
}
