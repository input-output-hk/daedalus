import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import sendLogsChoice from '../support/helpers/send-logs-choice-helpers';

const SEND_LOGS_CHOICE_FORM = '.SendLogsChoiceForm_component';

Given(/^I agree to send logs to remote server$/, async function () {
  await sendLogsChoice.agree(this.client);
});

Given(/^I am on the "Send logs choice" screen$/, function () {
  return this.client.waitForVisible('.SendLogsChoiceForm_component');
});

Given(/^I didnt choose send logs option$/, async function () {
  await this.client.execute(() => {
    daedalus.reset();
  });
});

When(/^I click on "Continue" button$/, function () {
  return this.waitAndClick('.SendLogsChoiceForm_component .acceptButton');
});

Then(/^I should not see the "Send logs choice" screen anymore$/, function () {
  return this.client.waitForVisible(SEND_LOGS_CHOICE_FORM, null, true);
});

Then(/^I should have "Send logs" accepted$/, async function () {
  const result = await this.client.executeAsync((done) => {
    daedalus.stores.profile.getSendLogsChoiceRequest.execute()
      .then(done)
      .catch((error) => done(error));
  });
  expect(result.value).to.equal(true);
});
