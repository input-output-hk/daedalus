import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import languageSelection from '../helpers/language-selection-helpers';

const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';

Given(/^I have selected English language$/, async function() {
  await languageSelection.ensureLanguageIsSelected(this.client, {
    language: 'en-US',
  });
});

Given(/^I dont have a language set$/, async function() {
  await this.client.execute(() => {
    daedalus.reset();
  });
});

When(/^I am on the language selection screen$/, function() {
  return this.client.waitForVisible('.LanguageSelectionForm_component');
});

When(/^I open language selection dropdown$/, function() {
  return this.waitAndClick(
    '.LanguageSelectionForm_component .SimpleInput_input'
  );
});

When(/^I select Japanese language$/, function() {
  return this.waitAndClick(
    '//*[@class="SimpleOptions_option"]//*[contains(text(), "Japanese")]'
  );
});

When(/^I submit the language selection form$/, function() {
  return this.waitAndClick('.LanguageSelectionForm_submitButton');
});

Then(/^I should not see the language selection screen anymore$/, function() {
  return this.client.waitForVisible(LANGUAGE_SELECTION_FORM, null, true);
});

Then(/^I should have Japanese language set$/, async function() {
  const result = await this.client.executeAsync(done => {
    daedalus.stores.profile.getProfileLocaleRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  expect(result.value).to.equal('ja-JP');
});
