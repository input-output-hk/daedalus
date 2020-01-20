// @flow
import { Given, Then } from 'cucumber';
import { navigateTo, waitUntilUrlEquals } from '../../../navigation/e2e/steps/helpers';
import { timeout } from '../../../common/e2e/steps/helpers';

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
    `//div[@class="WalletRow_title" and starts-with(text(), "Reward Wallet")]`
  );
  await this.client.waitForVisible(
    `//div[@class="WalletRow_title" and starts-with(text(), "Legacy Wallet")]`
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
  // Makes sure the wallet is not delegate, in case the test restarts
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

Given(/^I delegate the wallet$/, async function() {
  await this.client.execute((stakePoolId, walletId, passphrase) => {
    daedalus.actions.staking.joinStakePool.trigger({ stakePoolId, walletId, passphrase })
  }, pool.id, wallet.id, 'Secret1234');
})

Then(/^I should see the delegated pool name$/, async function() {
  await this.client.waitForVisible(`//span[text()="[${pool.ticker}]"]`);
})

Then(/^I should see the delegated menu$/, async function() {
  await this.client.waitForVisible('.DropdownMenu_dropdownToggle');
})




