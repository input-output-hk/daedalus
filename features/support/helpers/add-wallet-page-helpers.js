import { waitAndClick } from './shared-helpers';

const ADD_WALLET = '.WalletAdd';
const IMPORT_WALLET_BUTTON = '.importWalletButton';

export default {
  waitForVisible: (client, { isHidden } = {}) =>
    client.waitForVisible(ADD_WALLET, null, isHidden),
  clickImportButton: client =>
    waitAndClick(client, `${ADD_WALLET} ${IMPORT_WALLET_BUTTON}`),
};
