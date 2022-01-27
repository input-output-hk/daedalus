import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import BigNumber from "bignumber.js/bignumber";
import { delegationCentreStakingHelper, getStakePoolByRanking } from "./helpers";
import { getWalletByName } from "../../../wallets/e2e/steps/helpers";
import { formattedWalletAmount } from "../../../../source/renderer/app/utils/formatters";

const {
  stakingButtonVisible,
  clickStakingButton,
  delegationCenterVisible
} = delegationCentreStakingHelper;
const LOADING_MESSAGE_SELECTOR = '.StakePools_component.StakePools_isLoading .StakePools_loadingBlockWrapper p';
const STAKE_POOL_TAB_BUTTON = '.stake-pools.NavButton_component.NavButton_normal';
const STAKE_POOL_PAGE = '.StakePools_component';
const STAKE_POOLS_LIST_SELECTOR = '.StakePoolsList_component .ThumbPool_component:nth-child(1)';
const STAKE_POOLS_SEARCH_SELECTOR = '.StakePoolsSearch_component .StakePoolsSearch_searchInput.SimpleFormField_root input';
const SEARCH_RESULTS_LABEL_SELECTOR = '.StakePools_component h2 span';
const STAKE_POOL_SLUG_SELECTOR = '.StakePoolsList_component .ThumbPool_component:nth-child(3) .ThumbPool_ticker';
const STAKE_POOL_TOOLTIP_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible';
const STAKE_POOL_TOOLTIP_DESCRIPTION_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_description';
const STAKE_POOL_TOOLTIP_TICKER_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_ticker';
const STAKE_POOL_TOOLTIP_HOMEPAGE_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_homepage';
const STAKE_POOL_TOOLTIP_RANKING_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_ranking span';
const STAKE_POOL_TOOLTIP_PROFIT_MARGIN_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_profitMargin span';
const STAKE_POOL_TOOLTIP_COST_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_cost span';
const STAKE_POOL_TOOLTIP_PERFORMANCE_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_performance span';
const STAKE_POOL_TOOLTIP_NAME_SELECTOR = '.TooltipPool_component.TooltipPool_isVisible .TooltipPool_name';
const STAKE_POOL_TOOLTIP_BUTTON_SELECTOR = '//button[text()="Delegate to this pool"]';
const DELEGATE_WALLET_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsIntroDialog_delegationStepsIntroDialogWrapper';
const DIALOG_CONTINUE_SELECTOR = '//button[text()="Continue"]';
const DIALOG_CONFIRM_SELECTOR = '.DelegationSteps_delegationSteps .DialogCloseButton_component';
const DELEGATION_WALLET_FIRST_STEP_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseWalletDialog_delegationStepsChooseWalletDialogWrapper';
const DELEGATION_WALLET_SECOND_STEP_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseStakePoolDialog_delegationStepsChooseStakePoolDialogWrapper';
const DELEGATION_WALLET_DROPDOWN_SELECTOR = '.DelegationSteps_delegationSteps.DelegationStepsChooseWalletDialog_delegationStepsChooseWalletDialogWrapper .DelegationStepsChooseWalletDialog_walletSelect';
const SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR = '.DelegationStepsChooseStakePoolDialog_selectStakePoolLabel span';
const DELEGATION_WALLET_LAST_STEP_SELECTOR = '.DelegationSteps_content.DelegationStepsConfirmationDialog_content #spendingPassword--1';
const STAKE_POOLS_DELEGATING_LABEL = '.StakePools_component .StakePools_listTitle';
Given(/^I am on the Delegation Center screen/, async function () {
  await stakingButtonVisible(this.client);
  await clickStakingButton(this.client);
  await delegationCenterVisible(this.client);
});
Given(/^I set stake pools fetch failed$/, async function () {
  await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.staking.fakeStakePoolsLoading.trigger(true);
    done();
  });
});
Given(/^I have a wallet "([^"]*)" delegated to stake pool with rank "([^"]*)"$/, async function (walletName, stakePoolRank) {
  const wallet = await getWalletByName.call(this, walletName);
  const stakePool = await getStakePoolByRanking(this.client, stakePoolRank);
  await this.client.execute((stakePoolId, walletId, passphrase) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.staking.joinStakePool.trigger({
      stakePoolId,
      walletId,
      passphrase
    });
  }, stakePool.id, wallet.id, 'Secret1234');
});
When(/^Stake pools loading failed/, async function () {
  const stakePools = await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.staking.fakeStakePoolsLoading.trigger(true);
    done();
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  expect(result.length).to.equal(0);
});
When(/^I see the stake pools search input field/, function () {
  return this.client.waitForVisible(STAKE_POOLS_SEARCH_SELECTOR);
});
When(/^I enter "([^"]*)" in search input field/, function (data) {
  return this.client.setValue(STAKE_POOLS_SEARCH_SELECTOR, data);
});
When(/^I click on stake pool with order number "([^"]*)"/, function (rankNumber) {
  return this.waitAndClick(`.StakePoolsList_component .ThumbPool_component:nth-child(${rankNumber})`);
});
When(/^I click "continue" button/, async function () {
  await this.client.waitForEnabled(DIALOG_CONTINUE_SELECTOR);
  return this.waitAndClick(DIALOG_CONTINUE_SELECTOR);
});
When(/^I click "confirm" button/, function () {
  return this.waitAndClick(DIALOG_CONFIRM_SELECTOR);
});
Then(/^I should see stake pool tooltip with order number "([^"]*)"/, async function (rankNumber) {
  const stakePoolRankingText = await this.waitAndGetText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  const stakePoolRanking = Array.isArray(stakePoolRankingText) ? stakePoolRankingText[0] : stakePoolRankingText;
  expect(stakePoolRanking).to.equal(rankNumber);
});
When(/^I click on "Delegate to this pool"/, function () {
  return this.waitAndClick(STAKE_POOL_TOOLTIP_BUTTON_SELECTOR);
});
When(/^I enter staking pool spending password "([^"]*)" and click "confirm" button$/, async function (password) {
  await this.client.setValue(DELEGATION_WALLET_LAST_STEP_SELECTOR, password);
  return this.waitAndClick(DIALOG_CONFIRM_SELECTOR);
});
When(/^I click on delegated stake pool/, async function () {
  await this.waitAndClick('.StakePoolsList_component.stakePoolsDelegatingList .ThumbPool_component:nth-child(1)');
});
Then(/^Stake pool with rank "([^"]*)" tooltip shows correct data$/, async function (stakePoolRank) {
  const stakePool = await getStakePoolByRanking(this.client, stakePoolRank);
  await this.client.waitForVisible(STAKE_POOL_TOOLTIP_SELECTOR);
  const stakePoolDescription = await this.waitAndGetText(STAKE_POOL_TOOLTIP_DESCRIPTION_SELECTOR);
  const stakePoolTicker = await this.waitAndGetText(STAKE_POOL_TOOLTIP_TICKER_SELECTOR);
  const stakePoolHomepage = await this.waitAndGetText(STAKE_POOL_TOOLTIP_HOMEPAGE_SELECTOR);
  const stakePoolPerformanceText = await this.waitAndGetText(STAKE_POOL_TOOLTIP_PERFORMANCE_SELECTOR);
  const stakePoolCost = await this.waitAndGetText(STAKE_POOL_TOOLTIP_COST_SELECTOR);
  const stakePoolName = await this.waitAndGetText(STAKE_POOL_TOOLTIP_NAME_SELECTOR);
  const stakePoolProfitMargin = await this.waitAndGetText(STAKE_POOL_TOOLTIP_PROFIT_MARGIN_SELECTOR);
  const stakePoolRanking = await this.waitAndGetText(STAKE_POOL_TOOLTIP_RANKING_SELECTOR);
  const cost = new BigNumber(stakePool.cost.c);
  const formattedCost = formattedWalletAmount(cost, true, false);
  const stakePoolPerformance = Array.isArray(stakePoolPerformanceText) ? stakePoolPerformanceText[0] : stakePoolPerformanceText;
  expect(stakePool.ticker).to.equal(stakePoolTicker);
  expect(stakePool.homepage).to.equal(stakePoolHomepage);
  expect(`${stakePool.performance}%`).to.equal(stakePoolPerformance);
  expect(formattedCost).to.equal(stakePoolCost);
  expect(stakePool.description).to.equal(stakePoolDescription);
  expect(stakePool.name).to.equal(stakePoolName);
  expect(`${stakePool.profitMargin}%`).to.equal(stakePoolProfitMargin);
  expect(stakePool.ranking).to.equal(parseInt(stakePoolRanking, 10));
});
Then(/^I should see "Delegate Wallet" dialog/, function () {
  return this.client.waitForVisible(DELEGATE_WALLET_SELECTOR);
});
Then(/^I should see step 1 of 3 screen/, function () {
  return this.client.waitForVisible(DELEGATION_WALLET_FIRST_STEP_SELECTOR);
});
Then(/^I open the wallet dropdown/, function () {
  return this.waitAndClick(DELEGATION_WALLET_DROPDOWN_SELECTOR);
});
Then(/^I choose "([^"]*)" wallet$/, function (walletName) {
  this.walletName = walletName;
  return this.waitAndClick(`//*[text()[contains(.,"${walletName}")]]`);
});
Then(/^I should see step 2 of 3 screen/, function () {
  return this.client.waitForVisible(DELEGATION_WALLET_SECOND_STEP_SELECTOR);
});
Then(/^I see following label on the dialog: "([^"]*)"$/, async function (message) {
  const selectedStakePoolLabel = await this.waitAndGetText(SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR);
  expect(selectedStakePoolLabel.toString().split('.')[0]).to.equal(message);
});
Then(/^I see delegation status message for stake pool with rank "([^"]*)"$/, async function (stakePoolRank) {
  const stakePool = await getStakePoolByRanking(this.client, stakePoolRank);
  const selectedWallet = await this.client.executeAsync((walletName, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const wallet = daedalus.stores.wallets.getWalletByName(walletName);
    done(wallet);
  }, this.walletName);
  const {
    pendingDelegations
  } = selectedWallet.value;
  const messageWithPendingDelegation = `You are already pending delegation ${this.walletName} wallet to [${stakePool.ticker}] stake pool`;
  const messageWithNoPendingDelegation = `You are already delegating ${this.walletName} wallet to [${stakePool.ticker}] stake pool`;
  const selectedStakePoolLabel = await this.waitAndGetText(SELECTED_STAKE_POOLS_DELEGATION_WALLET_DIALOG_SELECTOR);
  expect(selectedStakePoolLabel.toString().split('.')[0]).to.oneOf([messageWithPendingDelegation, messageWithNoPendingDelegation]);
});
Then(/^I should see label: "([^"]*)"$/, async function (message) {
  const delegatedStakePoolLabel = await this.waitAndGetText(STAKE_POOLS_DELEGATING_LABEL);
  expect(delegatedStakePoolLabel).to.equal(message);
});
Then(/^I click on stake pools tab button/, async function () {
  return this.waitAndClick(STAKE_POOL_TAB_BUTTON);
});
Then(/^I am on the Staking pool screen/, async function () {
  await this.client.waitForVisible(STAKE_POOL_PAGE);
});
Then(/^I should't see loading message anymore/, function () {
  return this.client.waitForVisible(LOADING_MESSAGE_SELECTOR, null, true);
});
Then(/^I should see stake pools listed/, async function () {
  await this.client.waitForVisible(STAKE_POOLS_LIST_SELECTOR);
});
Then(/^I should not see any stake pool$/, async function () {
  return this.client.waitForVisible(STAKE_POOLS_LIST_SELECTOR, null, true);
});
Then(/^I should see stake pools ordered by rank$/, async function () {
  const stakePools = await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.staking.stakePoolsRequest.execute().then(done).catch(error => done(error));
  });
  const result = stakePools && stakePools.value ? stakePools.value : [];
  const orderCorrect = result.every(({
    ranking
  }, i) => parseInt(ranking, 10) === i + 1);
  expect(orderCorrect).to.be.true;
});
Then(/^I should see the following loading message:$/, async function (message) {
  await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.staking.fakeStakePoolsLoading.trigger();
    done();
  });
  const loadingMsg = message.hashes()[0];
  const loadingMsgOnScreen = await this.waitAndGetText(LOADING_MESSAGE_SELECTOR);
  const expectedLoadingMsg = await this.intl(loadingMsg.message);
  expect(loadingMsgOnScreen).to.equal(expectedLoadingMsg);
});
Then(/^I should see search label with text: "([^"]*)"$/, async function (message) {
  const searchResultsMessages = await this.waitAndGetText(SEARCH_RESULTS_LABEL_SELECTOR);
  expect(searchResultsMessages).to.equal(message);
});
Then(/^I should see stake pool with slug "([^"]*)"$/, async function (slug) {
  const stakePoolSlug = await this.waitAndGetText(STAKE_POOL_SLUG_SELECTOR);
  expect(stakePoolSlug).to.equal(slug);
});