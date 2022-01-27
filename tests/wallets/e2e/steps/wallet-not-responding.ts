import { When, Then } from "cucumber";
import { WalletSyncStateStatuses } from "../../../../source/renderer/app/domains/Wallet";

When(/^the "([^"]*)" wallet is not responding$/, async function (walletName) {
  await this.client.execute((walletName, status) => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const walletIndex: number = daedalus.stores.wallets.all.findIndex(wallet => wallet.name === walletName);
    const modifiedWallet: {
      name: string;
      syncState: Record<string, any>;
    } = {
      name: walletName,
      syncState: {
        status
      }
    };
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.api.ada.setTestingWallet(modifiedWallet, walletIndex);
  }, walletName, WalletSyncStateStatuses.NOT_RESPONDING);
});
Then(/^the "Not Responding" Overlay should be (hidden|visible)/, async function (state) {
  const shouldBeHidden = state === 'hidden';
  await this.client.waitForVisible('.NotResponding_component', null, shouldBeHidden);
});
Then(/^the wallet navigation should switch to the "summary" tab/, async function () {
  await this.client.waitForVisible('.WalletSummary_component');
});