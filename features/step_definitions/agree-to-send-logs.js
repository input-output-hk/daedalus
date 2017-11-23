import { expect } from 'chai';
import sendLogsChoice from '../support/helpers/send-logs-choice-helpers';

const SEND_LOGS_CHOICE_FORM = '.SendLogsChoiceForm_component';

export default function () {
  this.Given(/^I agree to send logs to remote server$/, async function () {
    await sendLogsChoice.agree(this.client);
  });

  this.Given(/^I am on the "Send logs choice" screen$/, function () {
    return this.client.waitForVisible('.SendLogsChoiceForm_component');
  });

  this.Given(/^I didnt choose send logs option$/, async function () {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.When(/^I click on "Continue" button$/, function () {
    return this.waitAndClick('.SendLogsChoiceForm_component .acceptButton');
  });

  this.Then(/^I should not see the "Send logs choice" screen anymore$/, function () {
    return this.client.waitForVisible(SEND_LOGS_CHOICE_FORM, null, true);
  });

  this.Then(/^I should have "Send logs" accepted$/, async function () {
    const result = await this.client.executeAsync((done) => {
      daedalus.stores.profile.getSendLogsChoiceRequest.execute()
        .then(done)
        .catch((error) => done(error));
    });
    expect(result.value).to.equal(true);
  });
}
