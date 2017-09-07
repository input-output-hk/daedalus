import { expect } from 'chai';

const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

export default function () {
  this.Given(/^I have accepted "Terms of use"$/, async () => {
    await this.client.waitForVisible(TERMS_OF_USE_FORM);
    await this.client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    return this.client.waitForVisible(TERMS_OF_USE_FORM, null, true);
  });

  this.Given(/^I didnt accept "Terms of use"$/, async () => {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.Given(/^I am on the "Terms of use" screen$/, () => (
    this.client.waitForVisible(TERMS_OF_USE_FORM)
  ));

  this.When(/^I click on "I agree with terms of use" checkbox$/, () => (
    this.waitAndClick('.TermsOfUseForm_component .SimpleCheckbox_root')
  ));

  this.When(/^I submit the "Terms of use" form$/, () => (
    this.waitAndClick('.TermsOfUseForm_submitButton')
  ));

  this.Then(/^I should not see the "Terms of use" screen anymore$/, () => (
    this.client.waitForVisible(TERMS_OF_USE_FORM, null, true)
  ));

  this.Then(/^I should have "Terms of use" accepted$/, async () => {
    const result = await this.client.executeAsync((done) => {
      daedalus.stores.app.getTermsOfUseAcceptanceRequest.execute()
        .then(done)
        .catch((error) => done(error));
    });
    expect(result.value).to.equal(true);
  });
}
