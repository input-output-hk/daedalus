import { Given, Then, When } from 'cucumber';
import { expect } from 'chai';
import { refreshClient } from '../support/helpers/app-helpers';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';

const UNRECOVERABLE = 'unrecoverable';
const WAS_CONNECTED = false;

Given(/^Daedalus is running$/, function () {
  expect(this.app.isRunning()).to.be.true;
});

When(/^I refresh the main window$/, async function () {
  await refreshClient(this.client);
});

Then(/^I should see the loading screen with "([^"]*)"$/, async function (message) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Loading_connecting h1',
    text: message
  });
});

When(/^I set the node to disconnected$/, async function () {
  await this.client.executeAsync((done) => {
    daedalus.stores.networkStatus._setDisconnected(WAS_CONNECTED)
      .then(() => daedalus.stores.networkStatus._handleCardanoNodeStateChange(UNRECOVERABLE))
      .then(done)
      .catch((error) => done(error));
  });
});

Then(/^I should see the loading screen with report connecting issue text "([^"]*)"$/, async function (message) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Loading_reportIssueText',
    text: message
  });
});
