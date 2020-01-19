// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { delegationCentreStakingHelper } from './helpers';
import type { Daedalus } from '../../../types';
import {getVisibleElementsCountForSelector} from "../../../common/e2e/steps/helpers";

declare var daedalus: Daedalus;

const {
  stakingButtonVisible,
  clickStakingButton,
  delegationCenterVisible,
  clickStakePoolsTab,
  stakingPoolVisible
} = delegationCentreStakingHelper;

const LOADING_MESSAGE_SELECTOR = '.StakePools_component.StakePools_isLoading .StakePools_loadingBlockWrapper p';
const STAKE_POOLS_LIST_SELECTOR = '.StakePoolsList_component';
const STAKE_POOLS_SEARCH_SELECTOR = '.StakePoolsSearch_component .StakePoolsSearch_searchInput.SimpleFormField_root input';
const SEARCH_RESULTS_LABEL_SELECTOR = '.StakePools_component h2 span';
const STAKE_POOL_SELECTOR = '.StakePoolsList_component .StakePoolThumbnail_component';
const STAKE_POOL_SLUG_SELECTOR = '.StakePoolsList_component .StakePoolThumbnail_component .StakePoolThumbnail_ticker';

Given(/^I am on the Delegation Centre staking page/, async function () {
  await stakingButtonVisible(this.client);
  await clickStakingButton(this.client);
  await delegationCenterVisible(this.client);
  await clickStakePoolsTab(this.client);
  await stakingPoolVisible(this.client);
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

Then(/^I should see the following loading message:$/, async function (message) {
  await this.client.executeAsync((done) => {
      daedalus.actions.staking.fakeStakePoolLoading.trigger();
      done();
    }
  );
  const loadingMsg = message.hashes()[0];
  const loadingSelector = LOADING_MESSAGE_SELECTOR;
  await this.client.waitForText(loadingSelector);
  const loadingMsgOnScreen = await this.client.getText(loadingSelector);
  const expectedLoadingMsg = await this.intl(loadingMsg.message);
  expect(loadingMsgOnScreen).to.equal(expectedLoadingMsg);
});

When(/^Stake pools loading failed/, async function () {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  expect(result.length).to.not.equal(0);
});

Then(/^I should see loading stake pools error message:$/, async function (message) {
  await this.client.executeAsync((done) => {
      daedalus.actions.staking.fakeStakePoolLoading.trigger(true);
      done();
    }
  );
  const loadingMsg = message.hashes()[0];
  const loadingSelector = LOADING_MESSAGE_SELECTOR;
  await this.client.waitForText(loadingSelector);
  const loadingMsgOnScreen = await this.client.getText(loadingSelector);
  const expectedLoadingMsg = await this.intl(loadingMsg.message);
  expect(loadingMsgOnScreen).to.equal(expectedLoadingMsg);
});

Then(/^I should not see any stake pool/, function () {
  return this.client.waitForVisible(STAKE_POOLS_LIST_SELECTOR, null, true);
});

When(/^I see the stake pools search input field/, function () {
  return this.client.waitForVisible(STAKE_POOLS_SEARCH_SELECTOR);
});

When(/^I enter "([^"]*)" in search input field$/, function (data) {
  return this.client.setValue(STAKE_POOLS_SEARCH_SELECTOR, data);
});

Then(/^I should see message "([^"]*)"$/, async function (message) {
  const searchResultsLabelSelector = SEARCH_RESULTS_LABEL_SELECTOR;
  await this.client.waitForText(searchResultsLabelSelector);
  const searchResultsMessages = await this.client.getText(searchResultsLabelSelector);
  expect(searchResultsMessages).to.equal(message);
});

Then(/^I should see "([^"]*)" stake pool with slug "([^"]*)"$/, async function (numberOfStakePools, slug) {
  const stakePoolsCount = await getVisibleElementsCountForSelector(
    this.client,
    STAKE_POOL_SELECTOR
  );
  const stakePoolSlugSelector = STAKE_POOL_SLUG_SELECTOR;
  await this.client.waitForText(stakePoolSlugSelector);
  const stakePoolSlug = await this.client.getText(stakePoolSlugSelector);
  expect(stakePoolsCount).to.equal(parseInt(numberOfStakePools));
  expect(stakePoolSlug[0]).to.equal(slug);
});
