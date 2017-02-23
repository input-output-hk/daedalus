import PropTypes from 'prop-types';
import { PropTypes as MobxPropTypes } from 'mobx-react';
import defineActions from './lib/actions';

export default defineActions({
  startWalletBackup: {},
  initiateWalletBackup: {
    recoveryPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
  },
  acceptPrivacyNoticeForWalletBackup: {},
  continueToRecoveryPhraseForWalletBackup: {},
  addWordToWalletBackupVerification: {
    word: PropTypes.string.isRequired
  },
  clearEnteredRecoveryPhrase: {},
  acceptWalletBackupTermDevice: {},
  acceptWalletBackupTermRecovery: {},
  restartWalletBackup: {},
  cancelWalletBackup: {},
  finishWalletBackup: {},
});
