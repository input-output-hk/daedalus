import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { termsOfUseHelpers } from "./helpers";

const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';
const {
  acceptTerms
} = termsOfUseHelpers;
Given(/^I have accepted "Terms of use"$/, async function () {
  await acceptTerms(this.client);
});
Given(/^I didnt accept "Terms of use"$/, async function () {
  await this.client.execute(() => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.reset();
  });
});
Given(/^I am on the "Terms of use" screen$/, function () {
  return this.client.waitForVisible(TERMS_OF_USE_FORM);
});
When(/^I click on "I agree with terms of service" checkbox$/, function () {
  return this.waitAndClick('.TermsOfUseForm_component .SimpleCheckbox_root');
});
When(/^I submit the "Terms of use" form$/, function () {
  return this.waitAndClick('.TermsOfUseForm_submitButton');
});
Then(/^I should not see the "Terms of use" screen anymore$/, function () {
  return this.client.waitForVisible(TERMS_OF_USE_FORM, null, true);
});
Then(/^I should have "Terms of use" accepted$/, async function () {
  const result = await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.profile.getTermsOfUseAcceptanceRequest.execute().then(done).catch(error => done(error));
  });
  expect(result.value).to.equal(true);
});