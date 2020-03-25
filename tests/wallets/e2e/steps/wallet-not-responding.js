// @flow
import { When, Then } from 'cucumber';
import type { Daedalus } from '../../../types';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet.js';

declare var daedalus: Daedalus;

When(/^the "([^"]*)" wallet is not responding$/, async function(walletName) {
  await this.client.execute((walletName) => {
    const walletIndex: number = daedalus.stores.wallets.all.findIndex(wallet => wallet.name === walletName);
    const modifiedWallet: {
      name: string,
      syncState: Object;
    } = {
      name: `Test wallet - not working`,
      syncState: {
        status: WalletSyncStateStatuses.NOT_RESPONDING,
      },
    };
    daedalus.api.ada.setTestingWallet(modifiedWallet, walletIndex);
  }, walletName);
});


Then(/^I should see the "Not Responding" Overlay/, async function() {
  await this.client.waitForVisible('.NotResponding_component');
});

Then(/^the wallet navigation should switch to the "summary" tab/, async function() {
  await this.client.waitForVisible('.WalletSummary_component');
});





