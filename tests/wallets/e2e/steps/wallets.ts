import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { createWallets, getWalletByName, waitUntilWalletIsLoaded, restoreWalletWithFunds, restoreLegacyWallet, waitUntilUrlEquals, navigateTo, getWalletType, restoreWallet } from "./helpers";
// Create shelley or byron wallets
Given(/^I have (created )?the following (byron )?wallets:$/, async function (mode, _type, table) {
  const type = await getWalletType.call(this, _type);
  const isLegacy = type === 'byron';
  const sequentially = mode === 'created ';
  const wallets = table.hashes();
  await createWallets.call(this, wallets, {
    sequentially,
    isLegacy
  });
  // Ensure that ALL wallets are loaded
  await Promise.all(wallets.map(async (wallet) => await waitUntilWalletIsLoaded.call(this, wallet.name)));
});
// Restore a wallet of any kind
Given(/^I have restored the "([^"]*)" wallet of "([^"]*)" kind, "([^"]*)" subkind and "([^"]*)" recovery phrase$/, {
  timeout: 60000
}, async function (walletName, kind, subkind, recovery_phrase) {
  await restoreWallet.call(this, walletName, kind, subkind, recovery_phrase);
  await waitUntilWalletIsLoaded.call(this, walletName);
});
// Create a single wallet with funds
Given(/^I have a "([^"]*)" (byron )?wallet with funds$/, async function (walletName, _type) {
  const type = await getWalletType.call(this, _type);

  if (type === 'shelley') {
    await restoreWalletWithFunds(this.client, {
      walletName
    });
  } else {
    await restoreLegacyWallet(this.client, {
      walletName,
      hasFunds: true
    });
  }

  await waitUntilWalletIsLoaded.call(this, walletName);
});
// Create a single wallet with no funds
Given(/^I have a "([^"]*)" (byron )?wallet$/, async function (walletName, _type) {
  const type = await getWalletType.call(this, _type);
  const isLegacy = type === 'byron';

  if (!isLegacy) {
    await createWallets.call(this, [{
      name: walletName
    }], {});
  } else {
    await restoreLegacyWallet(this.client, {
      walletName,
      hasFunds: false
    });
  }

  await waitUntilWalletIsLoaded.call(this, walletName);
});
Given(/^I have a "([^"]*)" byron wallet for transfering funds$/, async function (walletName) {
  await restoreLegacyWallet(this.client, {
    walletName,
    hasFunds: true,
    transferFunds: true
  });
  await waitUntilWalletIsLoaded.call(this, walletName);
});
Given(/^I am on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screen) {
  const proceedToScreen = async () => {
    const wallet = await waitUntilWalletIsLoaded.call(this, walletName);

    if (wallet) {
      await navigateTo.call(this, `/wallets/${wallet.id}/${screen}`);
    } else {
      setTimeout(proceedToScreen, 500);
    }
  };

  await proceedToScreen();
});
Given('I have {int} restored (byron )?wallets', async function (numberOfWallets, _type) {
  const type = await getWalletType.call(this, _type);
  const isLegacy = type === 'byron';
  const wallets = [...Array(numberOfWallets)].map((x, i) => ({
    name: `Wallet ${i + 1}`,
    password: 'Secret1234'
  }));
  await createWallets.call(this, wallets, {
    isLegacy
  });
});
When(/^I have one wallet address$/, function () {
  return this.client.waitForVisible('.receiveAddress-1');
});
When(/^I enter spending password "([^"]*)"$/, function (password) {
  return this.client.setValue('.WalletReceiveRandom_spendingPassword input', password);
});
When(/^I click the wallet (.*) button$/, async function (buttonName) {
  const buttonSelector = `.NavButton_component.${buttonName}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});
When(/^I submit the create wallet dialog with the following inputs:$/, async function (table) {
  const fields = table.hashes()[0];
  await this.client.setValue('.WalletCreateDialog .walletName input', fields.walletName);
  return this.client.click('.WalletCreateDialog .primary');
});
When(/^I click continue$/, function () {
  return this.waitAndClick('.primary');
});
When(/^I click close$/, function () {
  return this.waitAndClick('.primary');
});
Then(/^I should have newly created "([^"]*)" wallet loaded$/, async function (walletName) {
  await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.wallets.walletsRequest.execute().then(done).catch(error => done(error));
  });
  const wallet = await getWalletByName.call(this, walletName);
  expect(wallet).to.be.an('object');
});
Then(/^I should be on some wallet page$/, async function () {
  return this.client.waitForVisible('.Navigation_component');
});
Then(/^I should be on the "([^"]*)" wallet "([^"]*)" screen$/, async function (walletName, screenName) {
  const wallet = await getWalletByName.call(this, walletName);
  return waitUntilUrlEquals.call(this, `/wallets/${wallet.id}/${screenName}`);
});
Then(/^I should be on the "([^"]*)" screen$/, async function (screenName) {
  return waitUntilUrlEquals.call(this, `/${screenName}`);
});
// Extended timeout is used for this step as it takes more than DEFAULT_TIMEOUT
// for the receiver wallet's balance to be updated on the backend after creating transactions
Then(/^the balance of "([^"]*)" wallet should be:$/, {
  timeout: 60000
}, async function (walletName, table) {
  const expectedData = table.hashes()[0];
  const receiverWallet = await getWalletByName.call(this, walletName);
  return this.client.waitUntil(async () => {
    const receiverWalletByron = await this.waitAndGetText(`.SidebarWalletsMenu_wallets .Wallet_${receiverWallet.id} .SidebarWalletMenuItem_info`);
    return receiverWalletByron === `${expectedData.balance} ADA`;
  }, 60000);
});
Then(/^"Byron" wallet badge should be visible in the wallet sidebar$/, async function () {
  return this.client.waitForVisible('.SidebarWalletMenuItem_active .LegacyBadge_component');
});
Then(/^I should see the following error messages on the wallet restore dialog:$/, async function (data) {
  let errorsOnScreen = await this.waitAndGetText('.ConfigurationDialog_error');
  if (typeof errorsOnScreen === 'string') errorsOnScreen = [errorsOnScreen];
  const errors = data.hashes();

  for (let i = 0; i < errors.length; i++) {
    const expectedError = await this.intl(errors[i].message);
    expect(errorsOnScreen[i]).to.equal(expectedError);
  }
});