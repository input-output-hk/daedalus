// @flow
import { waitAndClick } from '../../../features/tests/e2e/helpers/shared-helpers';
import { WalletSyncStateTags } from '../../../source/renderer/app/domains/Wallet';

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';

export const addWalletHelpers = {
  waitForVisible: (client, { isHidden } = {}) =>
    client.waitForVisible(ADD_WALLET, null, isHidden),
  clickImportButton: client =>
    waitAndClick(client, `${ADD_WALLET} ${IMPORT_WALLET_BUTTON}`),
};

export const isActiveWalletBeingRestored = async client => {
  const result = await client.execute(
    expectedSyncTag =>
      daedalus.stores.wallets.active.syncState.tag === expectedSyncTag,
    WalletSyncStateTags.RESTORING
  );
  return result.value;
};
