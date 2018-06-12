import { Given, When, Then } from 'cucumber';
import { getWalletByName } from '../support/helpers/wallets-helpers';
import { navigateTo } from '../support/helpers/route-helpers';

const NODE_UPDATE_COMPONENT = '.NodeUpdateNotification_component';
const TITLE_BAR = '.NodeUpdateNotification_titleBar';
const TOGGLE_BUTTON = '.NodeUpdateNotification_toggleButton';
const UPDATE_MESSAGE = '.NodeUpdateNotification_message';
const ACTIONS = '.NodeUpdateNotification_actions';
const ACCEPT_BTN = '.NodeUpdateNotification_acceptButton.SimpleButton_root';
const DENY_BTN = '.NodeUpdateNotification_denyButton.SimpleButton_root';

// BEGIN ACCEPT UPDATE -----------

Given(/^I am on the "([^"]*)" summary screen$/, async function (walletName) {
  const wallet = getWalletByName.call(this, walletName);
  await navigateTo.call(this, `/wallets/${wallet.id}/summary`);
});

When(/^I make a mock node update available$/, async function () {
  await this.client.executeAsync((nextVersion, done) => {
    daedalus.api.ada.setNextUpdate(nextVersion)
      .then(() => daedalus.stores.NodeUpdateStore.refreshNextUpdate())
      .then(done)
      .catch((error) => done(error));
  }, { version: 50 });
});

Then(/^I should see the unexpanded node update notification$/, function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${TITLE_BAR}`);
});

When(/^I click the node update notification's toggle button$/, async function () {
  const buttonSelector = `${NODE_UPDATE_COMPONENT} ${TITLE_BAR} ${TOGGLE_BUTTON}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});

Then(/^I should see the expanded node update notification's message$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${UPDATE_MESSAGE}`);
});

Then(/^I should see the expanded node update notification's accept button$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${ACCEPT_BTN}`);
});

Then(/^I should see the expanded node update notification's postpone button$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${DENY_BTN}`);
});

When(/^I click the accept button$/, async function () {
  await this.client.click(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${ACCEPT_BTN}`);
});

Then(/^I should not see the node update notification anymore$/, function () {
  return this.client.waitForVisible(NODE_UPDATE_COMPONENT, null, true);
});

// BEGIN POSTPONE UPDATE -----------

Given(/^I am on the "([^"]*)" summary screen$/, async function (walletName) {
  const wallet = getWalletByName.call(this, walletName);
  await navigateTo.call(this, `/wallets/${wallet.id}/summary`);
});

When(/^I make a mock node update available$/, async function () {
  await this.client.executeAsync((nextVersion, done) => {
    daedalus.api.ada.setNextUpdate(nextVersion)
      .then(() => daedalus.stores.NodeUpdateStore.refreshNextUpdate())
      .then(done)
      .catch((error) => done(error));
  }, { version: 50 });
});

Then(/^I should see the unexpanded node update notification$/, function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${TITLE_BAR}`);
});

When(/^I click the node update notification's toggle button$/, async function () {
  const buttonSelector = `${NODE_UPDATE_COMPONENT} ${TITLE_BAR} ${TOGGLE_BUTTON}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});

Then(/^I should see the expanded node update notification's message$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${UPDATE_MESSAGE}`);
});

Then(/^I should see the expanded node update notification's accept button$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${ACCEPT_BTN}`);
});

Then(/^I should see the expanded node update notification's postpone button$/, async function () {
  return this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${DENY_BTN}`);
});

When(/^I click the postpone button$/, async function () {
  await this.client.click(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${DENY_BTN}`);
});

Then(/^I should not see the node update notification anymore$/, function () {
  return this.client.waitForVisible(NODE_UPDATE_COMPONENT, null, true);
});
