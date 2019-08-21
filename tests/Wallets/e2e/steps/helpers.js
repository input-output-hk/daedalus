// @flow
import { expectTextInSelector, waitAndClick } from '../../../features/tests/e2e/helpers/shared-helpers';
import { WalletSyncStateTags } from '../../../source/renderer/app/domains/Wallet';

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';
const IMPORT_WALLET_DIALOG = '.WalletFileImportDialog';

export const addWalletHelpers = {
  waitForVisible: (client, { isHidden } = {}) =>
    client.waitForVisible(ADD_WALLET, null, isHidden),
  clickImportButton: client =>
    waitAndClick(client, `${ADD_WALLET} ${IMPORT_WALLET_BUTTON}`),
};

export const importWalletHelpers = {
  waitForDialog: (client, { isHidden } = {}) =>
    client.waitForVisible(IMPORT_WALLET_DIALOG, null, isHidden),
  clickImport: client =>
    waitAndClick(client, `${IMPORT_WALLET_DIALOG} .primary`),
  expectError: (client, { error }) =>
    expectTextInSelector(client, {
      selector: `${IMPORT_WALLET_DIALOG}_error`,
      text: error,
    }),
};

export const isActiveWalletBeingRestored = async client => {
  const result = await client.execute(
    expectedSyncTag =>
      daedalus.stores.wallets.active.syncState.tag === expectedSyncTag,
    WalletSyncStateTags.RESTORING
  );
  return result.value;
};
