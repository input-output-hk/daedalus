// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import MnemonicWord from './MnemonicWord';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletRecoveryPhraseEntryDialog.scss';

const messages = defineMessages({
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
    defaultMessage: '!!!I understand that my money are held securely on this device only, not on the company servers',
    description: 'Term and condition on wallet backup dialog describing that wallet is on a users device, not on company servers'
  },
  termRecovery: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.recovery',
    defaultMessage: `!!!I understand that if this application is moved to another device or deleted, my money can
    be only recovered with the backup phrase which were written down in a secure place`,
    description: 'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase'
  }
});

type Props = {
  recoveryPhraseShuffled: Array<{ word: string, isActive: boolean }>,
  enteredPhrase: Array<{ word: string }>,
  isValid: boolean,
  isTermDeviceAccepted: boolean,
  isTermRecoveryAccepted: boolean,
  isSubmitting: boolean,
  onAddWord: Function,
  canFinishBackup: boolean,
  onClear: Function,
  onAcceptTermDevice: Function,
  onAcceptTermRecovery: Function,
  onRestartBackup: Function,
  onCancelBackup: Function,
  onFinishBackup: Function,
};

@observer
export default class WalletRecoveryPhraseEntryDialog extends Component<Props> {

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
      isSubmitting,
      onAddWord,
      onClear,
      onAcceptTermDevice,
      onAcceptTermRecovery,
      canFinishBackup,
      onRestartBackup,
      onCancelBackup,
      onFinishBackup
    } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'WalletRecoveryPhraseEntryDialog',
    ]);

    const enteredPhraseString = enteredPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');

    const actions = [];

    actions.push({
      className: isSubmitting ? styles.isSubmitting : null,
      label: intl.formatMessage(messages.buttonLabelConfirm),
      onClick: onFinishBackup,
      disabled: !canFinishBackup,
      primary: true
    });

    // Only show "Clear" button when user is not yet done with entering mnemonic
    if (!isValid) {
      actions.unshift({
        label: intl.formatMessage(messages.buttonLabelClear),
        onClick: onClear,
      });
    }

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancelBackup}
        closeButton={<DialogCloseButton onClose={onCancelBackup} />}
        backButton={!isValid ? <DialogBackButton onBack={onRestartBackup} /> : null}
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
                key={index}
                word={word}
                index={index}
                isActive={isActive}
                onClick={(value) => isActive && onAddWord(value)}
              />
            ))}
          </div>
        )}

        {isValid && (
          <div>
            <div className={styles.checkbox}>
              <Checkbox
                label={<FormattedHTMLMessage {...messages.termDevice} />}
                onChange={onAcceptTermDevice}
                checked={isTermDeviceAccepted}
                skin={<SimpleCheckboxSkin />}
              />
            </div>
            <div className={styles.checkbox}>
              <Checkbox
                label={intl.formatMessage(messages.termRecovery)}
                onChange={onAcceptTermRecovery}
                checked={isTermRecoveryAccepted}
                skin={<SimpleCheckboxSkin />}
              />
            </div>
          </div>
        )}
      </Dialog>
    );
  }

}
