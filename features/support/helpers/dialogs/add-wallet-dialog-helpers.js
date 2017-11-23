import { waitAndClick } from '../shared-helpers';

const ADD_WALLET_DIALOG = '.WalletAddDialog';
const IMPORT_WALLET_BUTTON = '.importWalletButton';

export default {
  waitForDialog: (client, { isHidden } = {}) => (
    client.waitForVisible(ADD_WALLET_DIALOG, null, isHidden)
  ),
  clickImportButton: (client) => (
    waitAndClick(client, `${ADD_WALLET_DIALOG} ${IMPORT_WALLET_BUTTON}`)
  ),
};
