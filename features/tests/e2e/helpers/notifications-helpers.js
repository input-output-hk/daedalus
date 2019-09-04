import { WalletSyncStateStatuses } from '../../../../source/renderer/app/domains/Wallet';

export const isActiveWalletBeingRestored = async client => {
  const result = await client.execute(
    expectedSyncTag =>
      daedalus.stores.wallets.active.syncState.tag === expectedSyncTag,
    WalletSyncStateStatuses.RESTORING
  );
  return result.value;
};

export const waitForActiveRestoreNotification = (client, { isHidden } = {}) =>
  client.waitForVisible('.ActiveRestoreNotification', null, isHidden);
