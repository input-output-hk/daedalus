// @flow
import { Given, Then } from 'cucumber';
import { navigateTo, waitUntilUrlEquals } from '../../../navigation/e2e/steps/helpers';
import { timeout } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I am on the Delegation "([^"]*)" screen$/, async function(
  screenName
) {
  return navigateTo.call(this, `/staking/${screenName}`);
});

Given(/^the "([^"]*)" wallet is not delegating$/, async function(wallet) {
  // TODO: Improve this
  await timeout(7000);
  await this.client.executeAsync((walletName, passphrase, done) => {
    const {id: walletId, delegatedStakePoolId: stakePoolId } = daedalus.stores.wallets.getWalletByName(walletName);
    if (stakePoolId) {
      try {
        daedalus.actions.staking.quitStakePool
          .trigger({ stakePoolId, walletId, passphrase })
          .then(done)
      } catch(e) {
        done(new Error(e))
      }
    } else {
      done();
    }
  }, wallet, 'Secret1234');
})

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

// Then(/^the current and next epoch countdown have correct data$/, async function() {
// })

let wallet;
let pool;

Then(/^I should see the "delegate" option$/, async function() {
  // Makes sure the wallet is not delegating, in case the test restarts
  await this.client.waitUntil(async () => {
    const wallets = await this.client.execute(() => daedalus.stores.wallets.all);
    const pools = await this.client.execute(() => daedalus.stores.staking.stakePools);
    const isLoaded = wallets.value.length > 0 && pools.value.length > 0;
    if (!isLoaded) return false;
    pool = pools.value[0];
    wallet = wallets.value[0];
    try {
      await this.client.execute((stakePoolId, walletId, passphrase) => {
        daedalus.actions.staking.quitStakePool.trigger({ stakePoolId, walletId, passphrase })
      }, pool.id, wallet.id, 'Secret1234');
    } catch(e) {}
    return true;
  });
  await this.client.waitForVisible(`//span[@class="WalletRow_actionLink" and text()="Delegate"]`);
})

Given(/^My wallet was delegated$/, async function() {
  await this.client.execute((stakePoolId, walletId, passphrase) => {
    daedalus.actions.staking.joinStakePool.trigger({ stakePoolId, walletId, passphrase })
  }, pool.id, wallet.id, 'Secret1234');
})

Then(/^I should see the delegated pool name$/, async function() {
  await this.client.waitForVisible(`//span[text()="[${pool.ticker}]"]`);
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

Given(/^I start the wallet delegation process$/, async function() {
  await this.waitAndClick(
    '//span[@class="WalletRow_actionLink" and text()="Delegate"]'
  );
  await this.waitAndClick(
    '//button[text()="Continue"]'
  );
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
  await timeout(2000);
  await this.client.executeAsync((amount, sender, receiver, done) => {
    const walletSender = daedalus.stores.wallets.getWalletByName('Wallet Sender');
    const walletReceiver = daedalus.stores.wallets.getWalletByName('Wallet Receiver');
    daedalus.stores.addresses
      .getAddressesByWalletId(walletReceiver.id)
      .then(addresses => {
        daedalus.stores.wallets.sendMoneyRequest.execute({
          address: addresses[0].id,
          amount: amount * 1000000,
          passphrase: 'Secret1234',
          walletId: walletSender.id,
        }).then(done);
      });
  }, adaAmount, walletFrom, walletTo);
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



