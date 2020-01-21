// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { delegationCentreStakingHelper } from './helpers';
import type { Daedalus } from '../../../types';
import BigNumber from 'bignumber.js';

declare var daedalus: Daedalus;

const {
  stakingButtonVisible,
  clickStakingButton,
  delegationCenterVisible,
} = delegationCentreStakingHelper;

const LOADING_MESSAGE_SELECTOR = '.StakePools_component.StakePools_isLoading .StakePools_loadingBlockWrapper p';
const STAKE_POOL_TAB_BUTTON = '.stake-pools.NavButton_component.NavButton_normal';
const STAKE_POOL_PAGE = '.StakePools_component';
const STAKE_POOLS_LIST_SELECTOR = '.StakePoolsList_component';
const STAKE_POOLS_SEARCH_SELECTOR = '.StakePoolsSearch_component .StakePoolsSearch_searchInput.SimpleFormField_root input';
const SEARCH_RESULTS_LABEL_SELECTOR = '.StakePools_component h2 span';
const SECOND_STAKE_POOL_ITEM_SELECTOR = '.StakePoolsList_component .StakePoolThumbnail_component:nth-child(2)';
const STAKE_POOL_SLUG_SELECTOR = '.StakePoolsList_component .StakePoolThumbnail_component:nth-child(3) .StakePoolThumbnail_ticker';
const STAKE_POOL_TOOLTIP_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible';
const STAKE_POOL_TOOLTIP_DESCRIPTION_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_description';
const STAKE_POOL_TOOLTIP_TICKER_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_ticker';
const STAKE_POOL_TOOLTIP_HOMEPAGE_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_homepage';
const STAKE_POOL_TOOLTIP_RANKING_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_ranking span';
const STAKE_POOL_TOOLTIP_PROFIT_MARGIN_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_profitMargin span';
const STAKE_POOL_TOOLTIP_COST_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_cost span';
const STAKE_POOL_TOOLTIP_PERFORMANCE_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_performance span';
const STAKE_POOL_TOOLTIP_NAME_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible .StakePoolTooltip_name';
const STAKE_POOL_TOOLTIP_BUTTON_SELECTOR = '.StakePoolTooltip_component.StakePoolTooltip_isVisible button:last-child';
const DELEGATE_WALLET_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsIntroDialog_delegationStepsIntroDialogWrapper';
const DIALOG_CONTINUE_SELECTOR = '.DelegationSteps_delegationSteps .Dialog_actions .continueButton';
const DELEGATION_WALLET_FIRST_STEP_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseWalletDialog_delegationStepsChooseWalletDialogWrapper';
const DELEGATION_WALLET_SECOND_STEP_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseStakePoolDialog_delegationStepsChooseStakePoolDialogWrapper';
const DELEGATION_WALLET_DROPDOWN_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseWalletDialog_delegationStepsChooseWalletDialogWrapper .DelegationStepsChooseWalletDialog_walletSelect';
const SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR = '.DelegationStepsChooseStakePoolDialog_selectStakePoolLabel span';

Given(/^I am on the Delegation Center screen/, async function () {
  await stakingButtonVisible(this.client);
  await clickStakingButton(this.client);
  await delegationCenterVisible(this.client);
});

Then(/^I click on stake pools tab button/, async function () {
  return this.waitAndClick(STAKE_POOL_TAB_BUTTON);
});

Then(/^I am on the Staking pool screen/, async function () {
  return this.client.waitForVisible(STAKE_POOL_PAGE);
});

Then(/^I see "([^"]*)" stake pools$/, async function (numberOfStakePools) {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  expect(result.length).to.equal(parseInt(numberOfStakePools));
});

Then(/^I should see "([^"]*)" stake pools loaded by rank$/, async function (numberOfStakePools) {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  const orderCorrect = result.every(({ ranking }, i) => parseInt(ranking) === i + 1);
  expect(result.length).to.equal(parseInt(numberOfStakePools));
  expect(orderCorrect).to.be.true;
});

Then(/^I should see the following loading message:$/, async function (message) {
  await this.client.executeAsync((done) => {
      daedalus.actions.staking.fakeStakePoolLoading.trigger();
      done();
    }
  );
  const loadingMsg = message.hashes()[0];
  await this.client.waitForText(LOADING_MESSAGE_SELECTOR);
  const loadingMsgOnScreen = await this.client.getText(LOADING_MESSAGE_SELECTOR);
  const expectedLoadingMsg = await this.intl(loadingMsg.message);
  expect(loadingMsgOnScreen).to.equal(expectedLoadingMsg);
});

When(/^Stake pools loading failed/, async function () {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.actions.staking.fakeStakePoolLoading.trigger(true);
    done();
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  expect(result.length).to.equal(0);
});

Then(/^I should see loading stake pools error message:$/, async function (message) {
  const loadingMsg = message.hashes()[0];
  await this.client.waitForText(LOADING_MESSAGE_SELECTOR);
  const loadingMsgOnScreen = await this.client.getText(LOADING_MESSAGE_SELECTOR);
  const expectedLoadingMsg = await this.intl(loadingMsg.message);
  expect(loadingMsgOnScreen).to.equal(expectedLoadingMsg);
});

Then(/^I should not see any stake pool/, function () {
  return this.client.waitForVisible(STAKE_POOLS_LIST_SELECTOR, null, true);
});

When(/^I see the stake pools search input field/, function () {
  return this.client.waitForVisible(STAKE_POOLS_SEARCH_SELECTOR);
});

When(/^I enter "([^"]*)" in search input field/, function (data) {
  return this.client.setValue(STAKE_POOLS_SEARCH_SELECTOR, data);
});

Then(/^I should see message "([^"]*)"$/, async function (message) {
  await this.client.waitForText(SEARCH_RESULTS_LABEL_SELECTOR);
  const searchResultsMessages = await this.client.getText(SEARCH_RESULTS_LABEL_SELECTOR);
  expect(searchResultsMessages).to.equal(message);
});

Then(/^I should see number 3 stake pool with slug "([^"]*)"$/, async function (slug) {
  await this.client.waitForText(STAKE_POOL_SLUG_SELECTOR);
  const stakePoolSlug = await this.client.getText(STAKE_POOL_SLUG_SELECTOR);
  expect(stakePoolSlug).to.equal(slug);
});

When(/^I click on stake pool with order number 2/, function () {
  return this.waitAndClick(SECOND_STAKE_POOL_ITEM_SELECTOR);
});

Then(/^I should see stake pool tooltip with order number "([^"]*)"/, async function (positionOfStakePool) {
  const allStakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = allStakePools && allStakePools.value ? allStakePools.value : [];
  const secondStakePool = result[parseInt(positionOfStakePool) - 1];
  await this.client.waitForText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  const stakePoolRanking = await this.client.getText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  expect(secondStakePool.ranking.toString()).to.equal(stakePoolRanking);
});

Then(/^Stake pool "([^"]*)" tooltip shows correct data$/, async function (positionOfStakePool) {
  const stakePools = await this.client.executeAsync(done => {
    daedalus.stores.staking.stakePoolsRequest
      .execute()
      .then(done)
      .catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  const secondStakePool = result[parseInt(positionOfStakePool) - 1];
  await this.client.waitForVisible(STAKE_POOL_TOOLTIP_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_DESCRIPTION_SELECTOR);
  const stakePoolDescription = await this.client.getText(STAKE_POOL_TOOLTIP_DESCRIPTION_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_TICKER_SELECTOR);
  const stakePoolTicker = await this.client.getText(STAKE_POOL_TOOLTIP_TICKER_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_HOMEPAGE_SELECTOR);
  const stakePoolHomepage = await this.client.getText(STAKE_POOL_TOOLTIP_HOMEPAGE_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_PERFORMANCE_SELECTOR);
  const stakePoolPerformance = await this.client.getText(STAKE_POOL_TOOLTIP_PERFORMANCE_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_COST_SELECTOR);
  const stakePoolCost = await this.client.getText(STAKE_POOL_TOOLTIP_COST_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_NAME_SELECTOR);
  const stakePoolName = await this.client.getText(STAKE_POOL_TOOLTIP_NAME_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_PROFIT_MARGIN_SELECTOR);
  const stakePoolProfitMargin = await this.client.getText(STAKE_POOL_TOOLTIP_PROFIT_MARGIN_SELECTOR);
  await this.client.waitForText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  const stakePoolRanking = await this.client.getText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  expect(secondStakePool.ticker).to.equal(stakePoolTicker);
  expect(secondStakePool.homepage).to.equal(stakePoolHomepage);
  expect(`${secondStakePool.performance}%`).to.equal(stakePoolPerformance);
  expect(`${secondStakePool.cost.c[0]} ADA`).to.equal(stakePoolCost);
  expect(secondStakePool.description).to.equal(stakePoolDescription);
  expect(secondStakePool.name).to.equal(stakePoolName);
  expect(`${secondStakePool.profitMargin}%`).to.equal(stakePoolProfitMargin);
  expect(secondStakePool.ranking).to.equal(parseInt(stakePoolRanking));
});

When(/^I click on "([^"]*)"$/, async function (buttonLabel) {
  await this.client.waitForText(STAKE_POOL_TOOLTIP_BUTTON_SELECTOR);
  const selectedStakePoolLabel = await this.client.getText(STAKE_POOL_TOOLTIP_BUTTON_SELECTOR);
  expect(selectedStakePoolLabel).to.equal(buttonLabel);
  return this.waitAndClick(STAKE_POOL_TOOLTIP_BUTTON_SELECTOR);
});

Then(/^I should see "Delegate Wallet" dialog/, function () {
  return this.client.waitForVisible(DELEGATE_WALLET_SELECTOR);
});

When(/^I click "continue" button/, function () {
  return this.waitAndClick(DIALOG_CONTINUE_SELECTOR);
});

Then(/^I should see step 1 of 3 screen/, function () {
  return this.client.waitForVisible(DELEGATION_WALLET_FIRST_STEP_SELECTOR);
});

Then(/^I open the wallet dropdown/, function () {
  return this.waitAndClick(DELEGATION_WALLET_DROPDOWN_SELECTOR);
});

Then(/^I choose "([^"]*)"$/, function (walletName) {
  return this.waitAndClick(
    `//*[text()[contains(.,"${walletName}")]]`
  );
});

Then(/^I should see step 2 of 3 screen/, function () {
  return this.client.waitForVisible(DELEGATION_WALLET_SECOND_STEP_SELECTOR);
});

Then(/^I see following label on the dialog: "([^"]*)"$/, async function (message) {
  await this.client.waitForText(SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR);
  const selectedStakePoolLabel = await this.client.getText(SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR);
  expect(selectedStakePoolLabel.toString()).to.equal(message);
});
