// @flow
import { Given, Then } from 'cucumber';
import { navigateTo, waitUntilUrlEquals } from '../../../navigation/e2e/steps/helpers';

Given(/^I am on the Delegation "([^"]*)" screen$/, async function(
  screenName
) {
  return navigateTo.call(this, `/staking/${screenName}`);
});

Then(/^I should see a "Create rewards wallet" notification$/, async function() {
  await this.client.waitForVisible('.DelegationCenterNoWallets_component');
})


