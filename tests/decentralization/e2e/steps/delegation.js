// @flow
import { Given, Then } from 'cucumber';
import { navigateTo, waitUntilUrlEquals } from '../../../navigation/e2e/steps/helpers';
import { timeout } from '../../../common/e2e/steps/helpers';
import { getCurrentEpoch, getNextEpoch } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I am on the Delegation "([^"]*)" screen$/, async function(
  screenName
) {
  return navigateTo.call(this, `/staking/${screenName}`);
});

Then(/^I should see a "Create rewards wallet" notification$/, async function() {
  await this.client.waitForVisible('.DelegationCenterNoWallets_component');
})

Then(/^I should only see Reward wallets listed$/, async function() {
  await this.client.waitForVisible(
    '//div[@class="WalletRow_title" and starts-with(text(), "Reward Wallet")]'
  );
  await this.client.waitForVisible(
    '//div[@class="WalletRow_title" and starts-with(text(), "Legacy Wallet")]'
  , null, true);
})

Then(/^the current and next epoch countdown are correctly displayed$/, async function() {
  await this.client.waitForVisible('.DelegationCenterHeader_countdownContainer');
  await this.client.waitForVisible('.DelegationCenterHeader_heading');
  await this.client.waitForVisible('.DelegationCenterHeader_epochsContainer');
  await this.client.waitForVisible('.CountdownWidget_timeLeftContainer');
})

Then(/^the current and next epoch countdown have correct data$/, async function() {
  const currentEpoch = await getCurrentEpoch.call(this);
  const nextEpoch = await getNextEpoch.call(this);
  return nextEpoch === currentEpoch + 1;
})

let wallet;
let pool;

Then(/^the "([^"]*)" wallet should display the "([^"]*)" option$/, async function(walletName, optionText) {
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//following-sibling::div[@class="WalletRow_right"]//span[@class="WalletRow_actionLink" and text()="${optionText}"]`);
})

Given(/^the "([^"]*)" wallet was delegated to the first Stake Pool$/, async function(walletName) {
  await this.client.waitUntil(async () => {
    const { value: stakePoolsListIsLoaded} = await this.client.executeAsync((done) => done(daedalus.stores.staking.stakePools.length > 0));
    return stakePoolsListIsLoaded;
  });
  const data = await this.client.executeAsync((walletName, passphrase, done) => {
    const { id: walletId } = daedalus.stores.wallets.getWalletByName(walletName);
    const pool = daedalus.stores.staking.stakePools[0];
    const { id: stakePoolId } = pool;
    daedalus.actions.staking.joinStakePool.trigger({ stakePoolId, walletId, passphrase });
    done(pool);
  }, walletName, 'Secret1234');
  pool = data.value;
})

Then(/^the "([^"]*)" wallet should display the delegated Stake Pool ticker$/, async function(walletName) {
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//following-sibling::div[@class="WalletRow_right"]//span[text()="[${pool.ticker}]"]`);
})

Given(/^the "([^"]*)" wallet is undelegated$/, async function(wallet) {
  await this.client.executeAsync((walletName, passphrase, done) => {
    const { id: walletId } = daedalus.stores.wallets.getWalletByName(walletName);
    const pool = daedalus.stores.staking.stakePools[0];
    const { id: stakePoolId } = pool;
    daedalus.actions.staking.quitStakePool.trigger({ stakePoolId, walletId, passphrase });
    done(pool)
  }, wallet, 'Secret1234');
})

Then(/^I should see the delegated menu with "Change delegation" and "Undelegate" options$/, async function() {
  await this.waitAndClick('.DropdownMenu_dropdownToggle');
  await this.client.waitForVisible(
    '//span[@class="WalletRow_normalOption" and text()="Change stake pool"]'
  );
  await this.client.waitForVisible(
    '//span[@class="WalletRow_removeOption" and text()="Undelegate"]'
  );
})

Given(/^I start the wallet delegation process for the "([^"]*)" wallet$/, async function(walletName) {
  await this.waitAndClick(
    `//div[@class="WalletRow_title" and text()="${walletName}"]//parent::div//parent::div//span[@class="WalletRow_actionLink" and text()="Delegate"]`
  );
  await this.waitAndClick(
    '//button[text()="Continue"]'
  );
})

Given('I click the wallet selector', async function() {
  await this.waitAndClick('.SimpleFormField_inputWrapper');
})

Then(/^The "([^"]*)" wallet option should display the correct Stake Pool ticker$/, async function(walletName) {
  await this.client.waitForVisible(
    `//div[@class="WalletsDropdownOption_label" and text()="${walletName}"]//preceding-sibling::div[@class="WalletsDropdownOption_ticker" and contains(.,"${pool.ticker}")]`
  );
})

Then('I close the delegation process dialog', async function() {
  await this.waitAndClick('.DialogCloseButton_component');
})

Given(/^I sucessfully delegate my wallet$/, async function() {
  await this.waitAndClick('//span[@class="WalletRow_actionLink" and text()="Delegate"]');
  await timeout(2000);
  await this.waitAndClick('//button[text()="Continue"]');
  await timeout(2000);
  await this.waitAndClick('//button[text()="Continue"]');
  await timeout(2000);
  await this.waitAndClick('//button[text()="Continue"]');
  await this.waitAndClick('.StakePoolThumbnail_component');
  await this.waitAndClick('//button[text()="Continue"]');
  await this.client.waitForVisible('.SimpleInput_input');
  const input = this.client.element('.SimpleInput_input');
  input.setValue('Secret1234');
  await timeout(2000);
  this.client.click('.confirmButton');
  await this.waitAndClick('.closeButton');
})

Then(/^I should see a "([^"]*)" message$/, async function(message) {
  await this.client.waitForVisible(
    `//*[text()="${message}"]`
  );
})

Then(/^I close the wizard$/, async function() {
  await this.waitAndClick('.DialogCloseButton_component');
})

Given('I send {int} ADA from the {string} wallet to the {string} wallet', async function(adaAmount, walletFrom, walletTo) {
  const DATA = await this.client.executeAsync((amount, senderName, receiverName, done) => {
    const walletSender = daedalus.stores.wallets.getWalletByName(senderName);
    const walletReceiver = daedalus.stores.wallets.getWalletByName(receiverName);
    daedalus.stores.addresses
      .getAddressesByWalletId(walletReceiver.id)
      .then(addresses => {
        daedalus.stores.wallets.sendMoneyRequest.execute({
          address: addresses[0].id,
          amount: amount * 1000000,
          passphrase: 'Secret1234',
          walletId: walletSender.id,
        }).then(done)
      })
  }, adaAmount, walletFrom, walletTo);
  await this.client.waitForVisible(`//div[@class="WalletRow_title" and text()="${walletTo}"]//following-sibling::div[@class="WalletRow_description"]//span`);
})

Then(/^I choose the "([^"]*)" wallet$/, async function(walletName) {
  await this.waitAndClick('.SimpleFormField_inputWrapper');
  await this.waitAndClick(`//div[@class="WalletsDropdownOption_label" and text()="${walletName}"]`);
  await this.waitAndClick('//button[text()="Continue"]');
})

Then(/^I choose the first stake pool$/, async function() {
  await this.waitAndClick('.StakePoolThumbnail_component');
  await this.waitAndClick('//button[text()="Continue"]');
})

Then(/^I enter "([^"]*)" as the spending password$/, async function(spendingPassword) {
  await this.client.waitForVisible('.SimpleInput_input');
  const input = this.client.element('.SimpleInput_input');
  input.setValue(spendingPassword);
  await timeout(2000);
  this.client.click('.confirmButton');
})




