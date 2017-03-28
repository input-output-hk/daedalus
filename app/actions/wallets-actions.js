import PropTypes from 'prop-types';
import defineActions from './lib/actions';

export default defineActions({
  create: {
    name: PropTypes.string.isRequired,
    currency: PropTypes.string.isRequired,
  },
  delete: {
    walletId: PropTypes.string.isRequired,
  },
  toggleCreateWalletDialog: {},
  toggleAddWallet: {},
  toggleWalletRestore: {},
  restoreWallet: {
    recoveryPhrase: PropTypes.string.isRequired,
    walletName: PropTypes.string.isRequired
  },
  toggleWalletKeyImportDialog: {},
  importWalletFromKey: {
    filePath: PropTypes.string.isRequired,
  },
  sendMoney: {
    receiver: PropTypes.string.isRequired,
    amount: PropTypes.string.isRequired,
  },
  setActiveWallet: {
    walletId: PropTypes.string.isRequired,
  },
  showWalletAddressCopyNotification: {},

});
