import PropTypes from 'prop-types';
import { PropTypes as MobxPropTypes } from 'mobx-react';
import defineActions from './lib/actions';

export default defineActions({
  login: {
    email: PropTypes.string.isRequired,
    passwordHash: PropTypes.string.isRequired,
  },
  updateProfileField: {
    field: PropTypes.string.isRequired,
    value: PropTypes.string.isRequired,
  },
  goToRoute: {
    route: PropTypes.string.isRequired,
  },
  createPersonalWallet: {
    name: PropTypes.string.isRequired,
    currency: PropTypes.string.isRequired,
  },
  filterTransactions: {
    searchTerm: PropTypes.string.isRequired,
  },
  loadMoreTransactions: {},
  changeSidebarRoute: {
    route: PropTypes.string.isRequired,
  },
  toggleSidebar: {},
  toggleCreateWalletDialog: {},
  sendMoney: {
    title: PropTypes.string.isRequired,
    receiver: PropTypes.string.isRequired,
    amount: PropTypes.string.isRequired,
    description: PropTypes.string,
  },
  resizeWindow: {
    width: PropTypes.number.isRequired,
    height: PropTypes.number.isRequired,
  },
  initiateWalletBackup: {
    recoveryPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
  },
  acceptPrivacyNoticeForWalletBackup: {},
  continueToRecoveryPhraseForWalletBackup: {},
  startWalletBackup: {},
  addWordToWalletBackupVerification: {
    word: PropTypes.string.isRequired
  },
  clearEnteredRecoveryPhrase: {},
  acceptWalletBackupTermDevice: {},
  acceptWalletBackupTermRecovery: {},
  restartWalletBackup: {},
  cancelWalletBackup: {},
  finishWalletBackup: {},
  redeemAda: {
    certificate: PropTypes.instanceOf(File).isRequired,
    token: PropTypes.string.isRequired,
    walletId: PropTypes.string.isRequired,
  },
  adaSuccessfullyRedeemed: {},
  toggleAddWallet: {},
  toggleWalletRestore: {},
  restoreWallet: {
    recoveryPhrase: PropTypes.string.isRequired,
    walletName: PropTypes.string.isRequired
  }
}, PropTypes.validateWithErrors);
