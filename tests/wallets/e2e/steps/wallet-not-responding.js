// @flow
import { When, Then } from 'cucumber';
import type { Daedalus } from '../../../types';
import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet.js';

declare var daedalus: Daedalus;

When(/^the "([^"]*)" wallet is not responding$/, async function(walletName) {
  await this.client.execute((walletName, status) => {
    const walletIndex: number = daedalus.stores.wallets.all.findIndex(wallet => wallet.name === walletName);
    const modifiedWallet: {
      name: string,
      syncState: Object;
    } = {
      name: 'Test wallet - not responding',
      syncState: {
        status,
      },
    };
    daedalus.api.ada.setTestingWallet(modifiedWallet, walletIndex);
  }, walletName, WalletSyncStateStatuses.NOT_RESPONDING);
});

Then(/^the "Not Responding" Overlay should be (hidden|visible)/, async function(state) {
  const shouldBeHidden = state === 'hidden';
  await this.client.waitForVisible('.NotResponding_component', null, shouldBeHidden);
});

Then(/^the wallet navigation should switch to the "summary" tab/, async function() {
  await this.client.waitForVisible('.WalletSummary_component');
});
