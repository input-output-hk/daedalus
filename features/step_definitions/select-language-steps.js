export default function () {
  this.Given(/^I have selected English language$/, function () {
    // this.client.execute(locale => {
    //   daedalus.actions.profile.updateLocale({ locale });
    // }, 'en-US');
  });

  this.Given(/^I am on the language selection screen$/, async function () {
    await this.navigateTo('/profile/language-selection');
  });

  this.When(/^I see the language selection form$/, function () {
    return this.client.waitForVisible('.LanguageSelectionForm_component');
  });

  this.When(/^I open language selection dropdown$/, async function () {
    await this.waitAndClick('.LanguageSelectionForm_component .input_inputElement');
  });

  this.When(/^I select Japanese language$/, async function () {
    await this.client.click('.dropdown_values:nth-child(2)');
  });

  this.When(/^I submit the language selection form$/, async function () {
    const submitButton = '.LanguageSelectionForm_submitButton';
    await this.client.waitForVisible(submitButton);
    return this.client.click(submitButton);
  });
  this.Then(/^I should be on the Personal Wallet summary screen$/, async function () {
    return this.client.waitForVisible('.WalletNavigation_component');
  });
};
