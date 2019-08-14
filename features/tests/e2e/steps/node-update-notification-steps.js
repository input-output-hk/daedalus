import { When, Then } from 'cucumber';
import { expect } from 'chai';

const NODE_UPDATE_COMPONENT = '.NodeUpdateNotification_component';
const TITLE_BAR = '.NodeUpdateNotification_titleBar';
const TOGGLE_BUTTON = '.NodeUpdateNotification_toggleButton';
const UPDATE_MESSAGE = '.NodeUpdateNotification_message';
const ACTIONS = '.NodeUpdateNotification_actions';
const ACCEPT_BTN = '.NodeUpdateNotification_acceptButton';
const DENY_BTN = '.NodeUpdateNotification_denyButton';

const UPDATE_VERSION = 50;

When(/^I make a node update available$/, async function() {
  await this.client.executeAsync(
    (nextVersion, done) => {
      daedalus.api.ada
        .setNextUpdate(nextVersion)
        .then(() => daedalus.stores.NodeUpdateStore.refreshNextUpdate())
        .then(done)
        .catch(error => done(error));
    },
    { version: UPDATE_VERSION }
  );
});

Then(/^I should see the node update notification component$/, async function() {
  await this.client.waitForVisible(`${NODE_UPDATE_COMPONENT}`);
});

Then(/^I should see the notification's title bar$/, async function() {
  await this.client.waitForVisible(`${NODE_UPDATE_COMPONENT} ${TITLE_BAR}`);
});

Then(
  /^I should see the expected update version in the notification's title bar$/,
  async function() {
    const titleBarSelector = `${NODE_UPDATE_COMPONENT} ${TITLE_BAR}`;
    await this.client.waitForText(titleBarSelector);
    const versionText = await this.client.getText(titleBarSelector);
    const expectedVersionText = await this.intl(
      'cardano.node.update.notification.titleWithVersion',
      { version: UPDATE_VERSION }
    );
    expect(versionText).to.equal(expectedVersionText);
  }
);

Then(/^I should see the notification's toggle button$/, async function() {
  await this.client.waitForVisible(
    `${NODE_UPDATE_COMPONENT} ${TITLE_BAR} ${TOGGLE_BUTTON}`
  );
});

Then(/^I should see the notification's update message$/, async function() {
  await this.client.waitForVisible(
    `${NODE_UPDATE_COMPONENT} ${UPDATE_MESSAGE}`
  );
});

Then(/^I should see the notification's accept button/, async function() {
  await this.client.waitForVisible(
    `${NODE_UPDATE_COMPONENT} ${ACTIONS} ${ACCEPT_BTN}`
  );
});

Then(/^I should see the notification's postpone button$/, async function() {
  await this.client.waitForVisible(
    `${NODE_UPDATE_COMPONENT} ${ACTIONS} ${DENY_BTN}`
  );
});

When(/^I click the notification's postpone button$/, async function() {
  await this.waitAndClick(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${DENY_BTN}`);
});

When(/^I click the notification's accept button$/, async function() {
  await this.waitAndClick(`${NODE_UPDATE_COMPONENT} ${ACTIONS} ${ACCEPT_BTN}`);
});

Then(/^I should not see the notification component anymore$/, async function() {
  await this.client.waitForVisible(NODE_UPDATE_COMPONENT, null, true);
});

Then(/^I should see the Daedalus window close$/, async function() {
  // there is latency between the window closing and this test running, so setTimeout
  await setTimeout(async () => {
    const windowCount = await this.client.getWindowCount();
    expect(windowCount).to.equal(0);
  }, 1500);
});
