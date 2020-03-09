// @flow
import { Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Then(
  /^"Balance" wallet "([^"]*)" action should be visible in the top bar notification$/,
  async function(action) {
    await this.client.waitForVisible('.LegacyNotification_component');
    const notificationAction = await this.waitAndGetText('.LegacyNotification_actions button:nth-child(2)');
    expect(notificationAction).to.equal(action);
  }
);

Then(
  /^"Balance" wallet notification should not be displayed in the wallet top bar$/,
  async function() {
    return this.client.waitForVisible('.LegacyNotification_component', null, true);
  }
);
