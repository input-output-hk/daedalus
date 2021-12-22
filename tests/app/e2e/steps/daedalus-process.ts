import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { waitUntilTextInSelector } from "../../../common/e2e/steps/helpers";
import { refreshClient, waitForCardanoNodeToExit, waitForDaedalusToExit } from "./helpers";

const CONNECTING_TITLE = '.SyncingConnectingStatus_headline';
Given(/^Daedalus is running$/, function () {
  expect(this.app.isRunning()).to.equal(true);
});
Given('I am on the connecting screen', async function () {
  this.client.executeAsync(done => {
    // Simulate that there is no connection to cardano node
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setSyncProgress(0);

    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.networkStatus._updateNetworkStatus().then(done);
  });
  await this.client.waitForVisible('.SyncingConnectingStatus_connecting');
});
When(/^I refresh the main window$/, async function () {
  await refreshClient(this.client);
});
When(/^I close the main window$/, async function () {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  await this.client.execute(() => daedalus.stores.window.closeWindow());
});
Then(/^Daedalus process is not running$/, async function () {
  await waitForDaedalusToExit(this.client);
});
Then(/^Daedalus should quit$/, {
  timeout: 70000
}, async function () {
  await waitForCardanoNodeToExit(this.client);
  await waitForDaedalusToExit(this.client);
});
Then(/^I should see the loading screen with "([^"]*)"$/, async function (message) {
  await waitUntilTextInSelector(this.client, {
    selector: CONNECTING_TITLE,
    text: message
  });
});
Then(/^I should see the main UI/, function () {
  return this.client.waitForVisible('.SidebarLayout_component');
});
Given('I set the syncing progress to {int} percent', async function (percentage) {
  this.client.executeAsync((percentage, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setSyncProgress(percentage);

    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.networkStatus._updateNetworkStatus().then(done);
  }, percentage);
});
When('I reset the syncing progress', async function () {
  this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setSyncProgress(null);

    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.networkStatus._updateNetworkStatus().then(done);
  });
});
When(/^I disconnect app$/, function () {
  this.client.execute(() => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.networkStatus._setDisconnected(true);
  });
});