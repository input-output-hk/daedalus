// @flow
import { Given, When, Then } from 'cucumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;
const CARDANO_NODE_STATE = '.cardanoNodeState .DaedalusDiagnostics_layoutData';

Given(/^I open the "Diagnostic" screen$/, async function() {
  this.client.execute(() => daedalus.actions.app.openDaedalusDiagnosticsDialog.trigger());
  return this.client.waitForVisible('.DaedalusDiagnostics_component');
});

Then(/^I should see the Cardano Node state is "([^"]*)"$/, async function(
  message
) {
  await waitUntilTextInSelector(this.client, {
    selector: CARDANO_NODE_STATE,
    text: message,
  });
});

When(/^I click on the "Restart Cardano Node" button$/, function() {
  return this.client.click('.DaedalusDiagnostics_cardanoNodeStatusBtn');
});
