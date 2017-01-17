// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletRecoveryPhraseDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseDialog';

@inject('stores', 'actions') @observer
export default class WalletBackupPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        walletBackup: PropTypes.shape({
          walletId: PropTypes.string.isRequired,
          recoveryPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
          completed: PropTypes.bool.isRequired,
          enteredPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
          isEntering: PropTypes.bool.isRequired,
          isValid: PropTypes.bool.isRequired,
          isWalletBackupStartAccepted: PropTypes.bool.isRequired,
          countdownRemaining: PropTypes.number.isRequired
        }),
      }),
    }).isRequired,
    actions: PropTypes.shape({
      acceptWalletBackupStart: PropTypes.func.isRequired
    }).isRequired
  };

  render() {
    const {
      recoveryPhrase,
      enteredPhrase,
      isEntering,
      isValid,
      isWalletBackupStartAccepted,
      countdownRemaining
    } = this.props.stores.wallets.walletBackup;
    const { acceptWalletBackupStart } = this.props.actions;
    return (
      <WalletRecoveryPhraseDialog
        enteredPhrase={enteredPhrase}
        isEntering={isEntering}
        isValid={isValid}
        recoveryPhrase={recoveryPhrase.map(word => ({ word }))}
        isWalletBackupStartAccepted={isWalletBackupStartAccepted}
        onAcceptStartBackup={acceptWalletBackupStart}
        countdownRemaining={countdownRemaining}
        canBackupStart={countdownRemaining === 0 && isWalletBackupStartAccepted}
      />
    );
  }

}
