import { Given, Then } from "cucumber";
import { expect } from "chai";
import BigNumber from "bignumber.js";
import { navigateTo } from "../../../navigation/e2e/steps/helpers";
import { timeout, notFoundWalletsErrorMessage } from "../../../common/e2e/steps/helpers";
import { getCurrentEpoch, getNextEpoch } from "./helpers";

Given(/^I am on the Delegation "([^"]*)" screen$/, async function (screenName) {
  return navigateTo.call(this, `/staking/${screenName}`);
});
Then(/^I should see a "Create a Shelley wallet" notification$/, async function () {
  await this.client.waitForVisible('.DelegationCenterNoWallets_component');
});
Then(/^I should only see Reward wallets listed$/, async function () {
  await this.client.waitForVisible('//div[@class="WalletRow_title" and starts-with(text(), "Reward Wallet")]');
  await this.client.waitForVisible('//div[@class="WalletRow_title" and starts-with(text(), "Legacy Wallet")]', null, true);
});
Then(/^the current and next epoch countdown are correctly displayed$/, async function () {
  await this.client.waitForVisible('.DelegationCenterHeader_countdownContainer');
  await this.client.waitForVisible('.DelegationCenterHeader_heading');
  await this.client.waitForVisible('.DelegationCenterHeader_epochsContainer');
  await this.client.waitForVisible('.CountdownWidget_timeLeftContainer');
});
Then(/^the current and next epoch countdown have correct data$/, async function () {
  const currentEpoch = await getCurrentEpoch.call(this);
  const nextEpoch = await getNextEpoch.call(this);
  return nextEpoch === currentEpoch + 1;
});
let pool;
Then(/^the "([^"]*)" wallet should display the "([^"]*)" option$/, async function (walletName, optionText) {
  const selector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`;
  const visibleOption = await this.waitAndGetText(selector);
  expect(visibleOption).to.equal(optionText);
});
Given(/^the "([^"]*)" wallet was delegated to the first Stake Pool$/, async function (walletName) {
  await this.client.waitUntil(async () => {
    const {
      value: stakePoolsListIsLoaded
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = await this.client.executeAsync(done => done(daedalus.stores.staking.stakePools.length > 0));
    return stakePoolsListIsLoaded;
  });
  const data = await this.client.executeAsync((walletName, passphrase, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const pool = daedalus.stores.staking.stakePools[0];
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const wallet = daedalus.stores.wallets.getWalletByName(walletName);

    if (pool && wallet) {
      const {
        id: stakePoolId
      } = pool;
      const {
        id: walletId
      } = wallet;
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.staking.joinStakePool.trigger({
        stakePoolId,
        walletId,
        passphrase
      });
    }

    done(pool);
  }, walletName, 'Secret1234');
  pool = data.value;
});
Given(/^the "([^"]*)" wallet was delegated to a Stake Pool with no metadata$/, async function () {
  const walletWithNoMetadata = {
    id: 'walletWithNoMetadata',
    addressPoolGap: 0,
    name: 'Wallet - No Metadata',
    amount: new BigNumber(500),
    availableAmount: new BigNumber(500),
    reward: new BigNumber(500),
    passwordUpdateDate: new Date(),
    syncState: {
      status: {
        quantity: 10,
        unit: 'percentage'
      }
    },
    isLegacy: false,
    delegatedStakePoolId: 'stakePoolWithNoMetadata'
  };
  const stakePoolWithNoMetadata = {
    id: 'stakePoolWithNoMetadata',
    ticker: '',
    homepage: '',
    pledgeAddress: '',
    performance: 100,
    producedBlocks: 1,
    controlledStake: '100',
    cost: '0',
    description: 'Stake Pool with no metadata',
    isCharity: false,
    name: 'SP No Metadata',
    profitMargin: 0,
    ranking: 1,
    retiring: null,
    saturation: 999.9000161273987
  };
  await this.client.execute((wallet, stakePool) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingWallets([wallet]);
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingStakePools([stakePool]);
  }, walletWithNoMetadata, stakePoolWithNoMetadata);
});
Then(/^the "([^"]*)" wallet should display the delegated Stake Pool ticker$/, async function (walletName) {
  const selector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="SimpleTooltip_root TooltipOverrides_root"]//span[@class="WalletRow_ticker tickerText"]`;
  const visibleStakePoolTicker = await this.waitAndGetText(selector);
  expect(visibleStakePoolTicker).to.equal(`[${pool.ticker}]`);
});
Given(/^the "([^"]*)" wallet is undelegated$/, async function (wallet) {
  await this.client.executeAsync((walletName, passphrase, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const pool = daedalus.stores.staking.stakePools[0];
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const wallet = daedalus.stores.wallets.getWalletByName(walletName);

    if (pool && wallet) {
      const {
        id: stakePoolId
      } = pool;
      const {
        id: walletId
      } = wallet;
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.staking.quitStakePool.trigger({
        stakePoolId,
        walletId,
        passphrase
      });
    }

    done(pool);
  }, wallet, 'Secret1234');
});
Then(/^I hover "([^"]*)" wallet row$/, async function (walletName) {
  await this.waitAndClick(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div`);
});
Then(/^I should see the "([^"]*)" wallet with "Undelegate" and "Redelegate" actions$/, async function (walletName) {
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`);
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionUndelegate"]`);
});
Then(/^I should see the "([^"]*)" wallet as undelegated$/, async function (walletName) {
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`);
  const selector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_ticker tickerText"]`;
  const visibleStakePoolTicker = await this.waitAndGetText(selector);
  expect(visibleStakePoolTicker).to.equal('UNDELEGATED');
});
Given(/^I start the wallet delegation process for the "([^"]*)" wallet$/, async function (walletName) {
  const selector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`;
  await this.waitAndClick(selector);
  await this.waitAndClick('//button[text()="Continue"]');
});
Given('I click the wallet selector', async function () {
  await this.waitAndClick('.SimpleFormField_inputWrapper');
});
Then(/^I should not see delegation actions for "([^"]*)" wallet$/, async function (walletName) {
  const selector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`;
  await this.client.waitForVisible(selector, null, true);
});
Then(/^The "([^"]*)" wallet option should display the correct Stake Pool ticker$/, async function (walletName) {
  const selector = `//div[@class="WalletsDropdownOption_label" and text()="${walletName}"]//preceding-sibling::div[@class="WalletsDropdownOption_ticker"]`;
  const visibleStakePoolTicker = await this.waitAndGetText(selector);
  expect(visibleStakePoolTicker).to.equal(`[${pool.ticker}]`);
});
Then('I close the delegation process dialog', async function () {
  await this.waitAndClick('.DialogCloseButton_component');
});
Given(/^I sucessfully delegate my "([^"]*)" wallet$/, {
  timeout: 60000
}, async function (walletName) {
  const delegateActionSelector = `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionDelegate"]`;
  await this.waitAndClick(delegateActionSelector);
  const continueButtonSelector = '//button[text()="Continue"]';
  const confirmButtonSelector = '.confirmButton';
  // Intro step
  await this.client.waitForVisible(continueButtonSelector);
  await this.client.click(continueButtonSelector);
  // Choose wallet step
  await this.client.waitForVisible(continueButtonSelector);
  await this.client.waitForEnabled(continueButtonSelector);
  await this.client.click(continueButtonSelector);
  // Select pool step
  await this.waitAndClick('.ThumbPool_component');
  await this.client.waitForVisible(continueButtonSelector);
  await this.client.waitForEnabled(continueButtonSelector);
  await this.client.click(continueButtonSelector);
  // Enter password step
  await this.client.waitForVisible('.SimpleInput_input');
  const input = this.client.element('.SimpleInput_input');
  input.setValue('Secret1234');
  // Confirmation step
  await this.client.waitForVisible(confirmButtonSelector);
  await this.client.waitForEnabled(confirmButtonSelector);
  await this.client.click(confirmButtonSelector);
  await this.waitAndClick('.closeButton');
});
Then(/^I should see a "([^"]*)" message$/, async function (message) {
  await this.client.waitForVisible(`//*[text()="${message}"]`);
});
Then(/^I close the wizard$/, async function () {
  await this.waitAndClick('.DialogCloseButton_component');
});
Given('I send {int} ADA from the {string} wallet to the {string} wallet', async function (adaAmount, walletFrom, walletTo) {
  await this.client.executeAsync((amount, senderName, receiverName, done) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const walletSender = daedalus.stores.wallets.getWalletByName(senderName);
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const walletReceiver = daedalus.stores.wallets.getWalletByName(receiverName);

    if (!walletSender || !walletReceiver || !walletSender.id || !walletReceiver.id) {
      return done(new Error(notFoundWalletsErrorMessage));
    }

    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    return daedalus.stores.addresses.getAddressesByWalletId(walletReceiver.id).then(addresses => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.stores.wallets.sendMoneyRequest.execute({
        address: addresses[0].id,
        amount: amount * 1000000,
        passphrase: 'Secret1234',
        walletId: walletSender.id
      }).then(done);
    });
  }, adaAmount, walletFrom, walletTo);
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletTo}"]//following-sibling::div[@class="WalletRow_description"]//span`);
});
Then(/^I choose the "([^"]*)" wallet$/, async function (walletName) {
  await this.waitAndClick('.SimpleFormField_inputWrapper');
  await this.waitAndClick(`//div[@class="WalletsDropdownOption_label" and text()="${walletName}"]`);
  const selector = '//button[text()="Continue"]';
  await this.client.waitForVisible(selector);
  await this.client.waitForEnabled(selector);
  await this.client.click(selector);
});
Then(/^I choose the first stake pool$/, async function () {
  await this.waitAndClick('.ThumbPool_component');
  const selector = '//button[text()="Continue"]';
  await this.client.waitForEnabled(selector);
  await this.waitAndClick(selector);
});
Then(/^I enter "([^"]*)" as the spending password$/, async function (spendingPassword) {
  await this.client.waitForVisible('.SimpleInput_input');
  const input = this.client.element('.SimpleInput_input');
  input.setValue(spendingPassword);
  await timeout(2000);
  this.client.click('.confirmButton');
});
Then(/^I should see a "Loading stake pools" message until the Stake Pools are loaded$/, async function () {
  const {
    value: stakePoolsLength
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  } = await this.client.execute(() => daedalus.stores.staking.stakePools.length);

  if (!stakePoolsLength) {
    await this.client.waitForVisible('.DelegationCenterBody_isLoading');
  }
});
Then(/^I should see the following error messages on the delegation process dialog:$/, async function (data) {
  let errorsOnScreen = await this.waitAndGetText('.DelegationStepsConfirmationDialog_error');
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});