import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { createWallets, getWalletByName } from "./helpers";
import { MAX_ADA_WALLETS_COUNT } from "../../../../source/renderer/app/config/numbersConfig";
import { sidebarHelpers } from "../../../navigation/e2e/steps/helpers";

Given('I create wallets until I reach the maximum number permitted', async function () {
  const wallets = [...Array(MAX_ADA_WALLETS_COUNT)].map((x, i) => ({
    name: `Wallet ${i + 1}`,
    password: 'Secret1234'
  }));
  await createWallets.call(this, wallets);
});
When('I should see maximum number of wallets in the wallets list', async function () {
  await this.client.waitUntil(async () => {
    const walletMenuItems = await this.client.elements('.SidebarWalletMenuItem_component');
    return walletMenuItems.value.length === MAX_ADA_WALLETS_COUNT;
  });
});
When('I delete the last wallet', async function () {
  const wallet = await getWalletByName.call(this, 'Wallet 20');
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  await this.client.execute((walletId, isLegacy) => daedalus.actions.wallets.deleteWallet.trigger({
    walletId,
    isLegacy
  }), wallet.id, wallet.isLegacy);
  await this.client.waitUntil(async () => {
    const wallets = await this.client.elements('.SidebarWalletMenuItem_component');
    return wallets.value.length < MAX_ADA_WALLETS_COUNT;
  });
});
Then(/^the buttons in the Add Wallet screen should be (disabled|enabled)/, async function (state) {
  const isDisabled = state === 'disabled' ? 'true' : null;
  sidebarHelpers.clickAddWalletButton(this.client);
  await this.client.waitForVisible('.WalletAdd_buttonsContainer .BigButtonForDialogs_component');
  const buttonsAreDisabled = await this.client.getAttribute('.WalletAdd_buttonsContainer .BigButtonForDialogs_component', 'disabled');
  // Excludes the "Join shared wallet" button
  buttonsAreDisabled.splice(1, 1);
  expect(buttonsAreDisabled).to.be.an('array').that.include(isDisabled).and.not.include(!isDisabled);
});
Then('I should see a disclaimer saying I have reached the maximum number of wallets', async function () {
  const disclaimer = await this.waitAndGetText('.WalletAdd_notification');
  expect(disclaimer.replace(/\n/, ' ')).to.equal(`You have reached the maximum of ${MAX_ADA_WALLETS_COUNT} wallets. No more wallets can be added.`);
});