export default function () {
  this.Given(/^I am on the language selection screen$/, async function () {
    await this.navigateTo('/profile/language-selection');
    return this.client.waitForVisible(`.LanguageSelectionForm_component`);
  });

  this.When(/^I submit the language selection form$/, async function () {
    const submitButton = '.LanguageSelectionForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });
};
