import { expect } from 'chai';
import termsOfUse from '../support/helpers/terms-of-use-helpers';

const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

export default function () {
  this.Given(/^I have accepted "Terms of use"$/, async function () {
    await termsOfUse.acceptTerms(this.client);
  });

  this.Given(/^I didnt accept "Terms of use"$/, async function () {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.Given(/^I am on the "Terms of use" screen$/, function () {
    return this.client.waitForVisible(TERMS_OF_USE_FORM);
  });

  this.When(/^I click on "I agree with terms of use" checkbox$/, function () {
    return this.waitAndClick('.TermsOfUseForm_component .SimpleCheckbox_root');
  });

  this.When(/^I submit the "Terms of use" form$/, function () {
    return this.waitAndClick('.TermsOfUseForm_submitButton');
  });

  this.Then(/^I should not see the "Terms of use" screen anymore$/, function () {
    return this.client.waitForVisible(TERMS_OF_USE_FORM, null, true);
  });

  this.Then(/^I should have "Terms of use" accepted$/, async function () {
    const result = await this.client.executeAsync((done) => {
      daedalus.stores.profile.getTermsOfUseAcceptanceRequest.execute()
        .then(done)
        .catch((error) => done(error));
    });
    expect(result.value).to.equal(true);
  });
}
