// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-toolbox';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import CheckboxWithLongLabel from '../../widgets/CheckboxWithLongLabel';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import MnemonicWord from './MnemonicWord';
import styles from './WalletRecoveryPhraseDialog.scss';

const messages = defineMessages({
  recoveryPhrase: {
    id: 'wallet.backup.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  mnemonicInstructions: {
    id: 'wallet.backup.recovery.phrase.dialog.mnemonic.instructions',
    defaultMessage: `!!!On the following screen, you will see a set of X random words. This is 
    your wallet backup phrase. It can be entered in any version of Daedalus application in order 
    to back up or restore your wallet’s funds and private key.`,
    description: 'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.'
  },
  backupInstructions: {
    id: 'wallet.backup.recovery.phrase.dialog.backup.instructions',
    defaultMessage: `!!!Please, make sure you have carefully written down your recovery phrase somewhere safe. 
    You will need this phrase later for next use and recover. Phrase is case sensitive.`,
    description: 'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.'
  },
  verificationInstructions: {
    id: 'wallet.backup.recovery.phrase.dialog.verification.instructions',
    defaultMessage: '!!!Tap each word in the correct order to verify your recovery phrase',
    description: 'Instructions for verifying wallet recovery phrase on dialog for entering wallet recovery phrase.'
  },
  buttonLabelIHaveWrittenItDown: {
    id: 'wallet.backup.recovery.phrase.dialog.button.label.iHaveWrittenItDown',
    defaultMessage: '!!!Yes, I’ve written it down',
    description: 'Label for button "Yes, I’ve written it down" on wallet backup dialog'
  },
  buttonLabelContinue: {
    id: 'wallet.recovery.phrase.show.dialog.button.labelContinue',
    defaultMessage: '!!!Continue',
    description: 'Label for button "Continue" on wallet backup dialog'
  },
  buttonLabelConfirm: {
    id: 'wallet.recovery.phrase.show.dialog.button.labelConfirm',
    defaultMessage: '!!!Confirm',
    description: 'Label for button "Confirm" on wallet backup dialog'
  },
  buttonLabelClear: {
    id: 'wallet.recovery.phrase.show.dialog.button.labelClear',
    defaultMessage: '!!!Clear',
    description: 'Label for button "Clear" on wallet backup dialog'
  },
  termNobodyWatching: {
    id: 'wallet.backup.recovery.phrase.dialog.terms.and.condition.nobodyWatching',
    defaultMessage: `!!!Make sure nobody looks into your screen unless you want them to have access to your funds.`,
    description: 'Term and condition on wallet backup dialog describing that nobody should be watching when recovery phrase is shown'
  },
  termDevice: {
    id: 'wallet.backup.recovery.phrase.dialog.terms.and.condition.device',
    defaultMessage: `!!!I understand that my money are held securely on this device only, not on the company servers`,
    description: 'Term and condition on wallet backup dialog describing that wallet is on a users device, not on company servers'
  },
  termRecovery: {
    id: 'wallet.backup.recovery.phrase.dialog.terms.and.condition.recovery',
    defaultMessage: `!!!I understand that if this application is moved to another device or deleted, my money can 
    be only recovered with the backup phrase which were written down in a secure place`,
    description: 'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase'
  }
});

@observer
export default class WalletRecoveryPhraseShowDialog extends Component {

  static propTypes = {
    recoveryPhrase: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired
    })).isRequired,
    recoveryPhraseShuffled: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired,
      isActive: PropTypes.bool.isRequired
    })).isRequired,
    enteredPhrase: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired,
      isChosen: PropTypes.bool.isRequired
    })).isRequired,
    isEntering: PropTypes.bool.isRequired,
    isValid: PropTypes.bool.isRequired,
    isWalletBackupStartAccepted: PropTypes.bool.isRequired,
    isPrivacyNoticeAccepted: PropTypes.bool.isRequired,
    isTermDeviceAccepted: PropTypes.bool.isRequired,
    isTermRecoveryAccepted: PropTypes.bool.isRequired,
    onAcceptStartBackup: PropTypes.func.isRequired,
    onAddWord: PropTypes.func.isRequired,
    countdownRemaining: PropTypes.number.isRequired,
    canPhraseBeShown: PropTypes.bool.isRequired,
    canFinishBackup: PropTypes.bool.isRequired,
    onStartWalletBackup: PropTypes.func.isRequired,
    onClear: PropTypes.func.isRequired,
    onAcceptPrivacyNotice: PropTypes.func.isRequired,
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
      recoveryPhrase,
      recoveryPhraseShuffled,
      enteredPhrase,
      isEntering,
      isValid,
      isWalletBackupStartAccepted,
      isTermDeviceAccepted,
      isTermRecoveryAccepted,
      onAcceptStartBackup,
      countdownRemaining,
      canPhraseBeShown,
      onStartWalletBackup,
      onAddWord,
      onClear,
      onAcceptPrivacyNotice,
      onAcceptTermDevice,
      onAcceptTermRecovery,
      canFinishBackup,
      onRestartBackup,
      onCancelBackup,
      onFinishBackup,
      isPrivacyNoticeAccepted
    } = this.props;
    let instructions;
    if (!isPrivacyNoticeAccepted) {
      instructions = messages.mnemonicInstructions
    } else if (isEntering) {
      instructions = messages.verificationInstructions;
    } else {
      instructions = messages.backupInstructions
    }
    const recoveryPhraseString = recoveryPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');
    const enteredPhraseString = enteredPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');
    const phrase = isEntering ? enteredPhraseString : recoveryPhraseString;
    const countdownDisplay = countdownRemaining > 0 ? ` (${countdownRemaining})` : '';

    let actions;
    if (!canPhraseBeShown) {
      actions = [
        {
          label: this.context.intl.formatMessage(messages.buttonLabelContinue) + countdownDisplay,
          onClick: onAcceptPrivacyNotice,
          disabled: !canPhraseBeShown,
          primary: true,
        }
      ];
    } else if (isPrivacyNoticeAccepted && !isEntering) {
      actions = [
        {
          label: this.context.intl.formatMessage(messages.buttonLabelIHaveWrittenItDown),
          onClick: onStartWalletBackup,
          disabled: !isPrivacyNoticeAccepted,
          primary: true,
        }
      ];
    } else {
      actions = [
        {
          label: this.context.intl.formatMessage(messages.buttonLabelClear),
          onClick: onClear,
          className: 'dialog_buttonSecondary'
        },
        {
          label: this.context.intl.formatMessage(messages.buttonLabelConfirm),
          onClick: onFinishBackup,
          disabled: !canFinishBackup,
          primary: true,
          className: 'dialog_buttonPrimary',
        }
      ];
    }
    return (
      <Dialog
        title={intl.formatMessage(messages.recoveryPhrase)}
        actions={actions}
        active
        style={styles.component}
      >
        <WalletRecoveryInstructions instructionsText={intl.formatMessage(instructions)} />
        {canPhraseBeShown && (<WalletRecoveryPhraseMnemonic phrase={phrase} />)}
        {!isPrivacyNoticeAccepted && (
          <CheckboxWithLongLabel
            label={intl.formatMessage(messages.termNobodyWatching)}
            onChange={onAcceptPrivacyNotice}
            checked={isPrivacyNoticeAccepted}
          />
        )}
        {isPrivacyNoticeAccepted && !isEntering && (
          <CheckboxWithLongLabel
            label={intl.formatMessage(messages.termDevice)}
            onChange={onAcceptStartBackup}
            checked={isWalletBackupStartAccepted}
          />
        )}
        {isEntering && (
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
        {isEntering && (<DialogBackButton onBack={onRestartBackup} />)}
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
