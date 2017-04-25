import { expect } from 'chai';

export default function () {
  this.Given(/^I have accepted "Terms of use"$/, async function () {
    await this.client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
  });

  this.Given(/^I didnt accept "Terms of use"$/, async function () {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.Given(/^I am on the "Terms of use" screen$/, function () {
    return this.client.waitForVisible('.TermsOfUseForm_component');
  });

  this.When(/^I click on "I agree with terms of use" checkbox$/, function () {
    return this.waitAndClick('.TermsOfUseForm_component .CheckboxWithLongLabel_checkbox');
  });

  this.When(/^I submit the "Terms of use" form$/, function () {
    return this.waitAndClick('.TermsOfUseForm_submitButton');
  });

  this.Then(/^I should not see the "Terms of use" screen anymore$/, function () {
    return this.client.waitForVisible('.TermsOfUseForm_component', null, true);
  });

  this.Then(/^I should have "Terms of use" accepted$/, async function () {
    const result = await this.client.executeAsync(function(done) {
      daedalus.stores.app.getTermsOfUseAcceptanceRequest.execute().then(done);
    });
    expect(result.value).to.equal(true);
  });

};
