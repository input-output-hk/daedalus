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
  toggleSidebar: {},
  toggleMaximized: {},
  sidebarCategorySelected: {
    category: PropTypes.string.isRequired,
  },
  toggleCreateWalletDialog: {},
  sendMoney: {
    // title: PropTypes.string.isRequired,
    receiver: PropTypes.string.isRequired,
    amount: PropTypes.string.isRequired,
    // description: PropTypes.string,
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
  setRedemptionCertificate: {
    certificate: PropTypes.instanceOf(File).isRequired,
  },
  setRedemptionPassPhrase: {
    passPhrase: PropTypes.string.isRequired,
  },
  setRedemptionCode: {
    redemptionCode: PropTypes.string.isRequired,
  },
  redeemAda: {
    walletId: PropTypes.string.isRequired,
  },
  adaSuccessfullyRedeemed: {},
  closeAdaRedemptionSuccessOverlay: {},
  toggleAddWallet: {},
  toggleWalletRestore: {},
  restoreWallet: {
    recoveryPhrase: PropTypes.string.isRequired,
    walletName: PropTypes.string.isRequired
  },
  acceptNodeUpdate: {},
  postponeNodeUpdate: {},
  toggleNodeUpdateNotificationExpanded: {},
  toggleWalletKeyImportDialog: {},
  importWalletFromKey: {
    keyFile: PropTypes.instanceOf(File).isRequired,
  },
}, PropTypes.validateWithErrors);
