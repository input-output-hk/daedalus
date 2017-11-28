import { expect } from 'chai';
import languageSelection from '../support/helpers/language-selection-helpers';

const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';

export default function () {
  this.Given(/^I have selected English language$/, async function () {
    await languageSelection.ensureLanguageIsSelected(this.client, { language: 'en-US' });
  });

  this.Given(/^I dont have a language set$/, async function () {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.When(/^I am on the language selection screen$/, function () {
    return this.client.waitForVisible('.LanguageSelectionForm_component');
  });

  this.When(/^I open language selection dropdown$/, function () {
    return this.waitAndClick('.LanguageSelectionForm_component .SimpleInput_input');
  });

  this.When(/^I select Japanese language$/, function () {
    return this.waitAndClick('//li[contains(text(), "Japanese")]');
  });

  this.When(/^I submit the language selection form$/, function () {
    return this.waitAndClick('.LanguageSelectionForm_submitButton');
  });

  this.Then(/^I should not see the language selection screen anymore$/, function () {
    return this.client.waitForVisible(LANGUAGE_SELECTION_FORM, null, true);
  });

  this.Then(/^I should have Japanese language set$/, async function () {
    const result = await this.client.executeAsync((done) => {
      daedalus.stores.profile.getProfileLocaleRequest.execute()
        .then(done)
        .catch((error) => done(error));
    });
    expect(result.value).to.equal('ja-JP');
  });
}
