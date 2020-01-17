// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^The sidebar shows "Delegation Center" staking page icon/, function () {
  return this.client.waitForVisible('.SidebarCategory_stakingIcon');
});

When(/^I click on the "Delegation Center" staking page button/, function () {
  return this.waitAndClick('.SidebarCategory_component.staking');
});

Then(/^I see the "Delegation Center" staking page/, function () {
  return this.client.waitForVisible('.StakingWithNavigation_page');
});

When(/^I click on the "Stake Pools" tab/, function () {
  return this.waitAndClick('.stake-pools.NavButton_component.NavButton_normal');
});

Then(/^I see the "Stake Pools" page/, function () {
  return this.client.waitForVisible('.StakePools_component');
});

Then(/^I should see the "Loading stake pools" message/, async function () {
  await this.client.executeAsync(() => {
    daedalus.stores.staking.fetchingStakePoolsFailed = true;
  });
  return this.client.waitForVisible('.StakePools_component.isLoading');
});

Then(/^I should see "([^"]*)" stake pools loaded by rank$/, async function (numberOfStakePools) {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  expect(result.length).to.equal(parseInt(numberOfStakePools));
});
