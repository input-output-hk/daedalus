// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import MnemonicWord from './MnemonicWord';
import styles from './WalletRecoveryPhraseEntryDialog.scss';

const messages = defineMessages({
  recoveryPhrase: {
    id: 'wallet.backup.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  verificationInstructions: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.verification.instructions',
    defaultMessage: '!!!Tap each word in the correct order to verify your recovery phrase',
    description: 'Instructions for verifying wallet recovery phrase on dialog for entering wallet recovery phrase.'
  },
  buttonLabelConfirm: {
    id: 'wallet.recovery.phrase.show.entry.dialog.button.labelConfirm',
    defaultMessage: '!!!Confirm',
    description: 'Label for button "Confirm" on wallet backup dialog'
  },
  buttonLabelClear: {
    id: 'wallet.recovery.phrase.show.entry.dialog.button.labelClear',
    defaultMessage: '!!!Clear',
    description: 'Label for button "Clear" on wallet backup dialog'
  },
  termDevice: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.device',
    defaultMessage: `!!!I understand that my money are held securely on this device only, not on the company servers`,
    description: 'Term and condition on wallet backup dialog describing that wallet is on a users device, not on company servers'
  },
  termRecovery: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.recovery',
    defaultMessage: `!!!I understand that if this application is moved to another device or deleted, my money can 
    be only recovered with the backup phrase which were written down in a secure place`,
    description: 'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase'
  }
});

@observer
export default class WalletRecoveryPhraseEntryDialog extends Component {

  static propTypes = {
    recoveryPhraseShuffled: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired,
      isActive: PropTypes.bool.isRequired
    })).isRequired,
    enteredPhrase: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired
    })).isRequired,
    isValid: PropTypes.bool.isRequired,
    isTermDeviceAccepted: PropTypes.bool.isRequired,
    isTermRecoveryAccepted: PropTypes.bool.isRequired,
    onAddWord: PropTypes.func.isRequired,
    canFinishBackup: PropTypes.bool.isRequired,
    onClear: PropTypes.func.isRequired,
    onAcceptTermDevice: PropTypes.func.isRequired,
    onAcceptTermRecovery: PropTypes.func.isRequired,
    onRestartBackup: PropTypes.func.isRequired,
    onCancelBackup: PropTypes.func.isRequired,
    onFinishBackup: PropTypes.func.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      recoveryPhraseShuffled,
      enteredPhrase,
      isValid,
      isTermDeviceAccepted,
      isTermRecoveryAccepted,
      onAddWord,
      onClear,
      onAcceptTermDevice,
      onAcceptTermRecovery,
      canFinishBackup,
      onRestartBackup,
      onCancelBackup,
      onFinishBackup
    } = this.props;

    const enteredPhraseString = enteredPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');

    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabelClear),
        onClick: onClear,
      },
      {
        label: intl.formatMessage(messages.buttonLabelConfirm),
        onClick: onFinishBackup,
        disabled: !canFinishBackup,
        primary: true
      }
    ];
    return (
      <Dialog
        title={intl.formatMessage(messages.recoveryPhrase)}
        actions={actions}
        active
        style={styles.component}
      >
        {!isValid && (
          <WalletRecoveryInstructions
            instructionsText={intl.formatMessage(messages.verificationInstructions)}
          />
        )}

        <WalletRecoveryPhraseMnemonic phrase={enteredPhraseString} />

        {!isValid && (
          <div className={styles.words}>
            {recoveryPhraseShuffled.map(({ word, isActive }, index) => (
              <MnemonicWord
                word={word}
                key={index}
                isActive={isActive}
                onClick={onAddWord}
              />
            ))}
          </div>
        )}

        <DialogCloseButton onClose={onCancelBackup} />
        <DialogBackButton onBack={onRestartBackup} />
        {isValid && (
          <div>
            <CheckboxWithLongLabel
              label={intl.formatMessage(messages.termDevice)}
              onChange={onAcceptTermDevice}
              checked={isTermDeviceAccepted}
            />
            <CheckboxWithLongLabel
              label={intl.formatMessage(messages.termRecovery)}
              onChange={onAcceptTermRecovery}
              checked={isTermRecoveryAccepted}
            />
          </div>
        )}
      </Dialog>
    );
  }

}
