// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-toolbox';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import MnemonicWord from './MnemonicWord';
import styles from './WalletRecoveryPhraseDialog.scss';

const messages = defineMessages({
  recoveryPhrase: {
    id: 'wallet.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  backupInstructions: {
    id: 'wallet.recovery.phrase.dialog.backup.instructions',
    defaultMessage: `Please, make sure you have carefully written down your recovery phrase somewhere safe. 
    You will need this phrase later for next use and recover. Phrase is case sensitive.`,
    description: 'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.'
  },
  verificationInstructions: {
    id: 'wallet.recovery.phrase.dialog.verification.instructions',
    defaultMessage: 'Tap each word in the correct order to verify your recovery phrase',
    description: 'Instructions for verifying wallet recovery phrase on dialog for entering wallet recovery phrase.'
  },
  buttonLabel: {
    id: 'wallet.recovery.phrase.show.dialog.button.label',
    defaultMessage: 'Yes, I’ve written it down',
    description: 'Label for button "Yes, I’ve written it down" on the dialog that shows wallet recovery phrase.'
  },
  termDevice: {
    id: 'wallet.recovery.phrase.dialog.terms.and.condition.device',
    defaultMessage: `I understand that my money are held securely on this device only, not on the company servers`,
    description: 'Term and condition on wallet backup dialog describing that wallet is on a users device, not on company servers'
  },
  termRecovery: {
    id: 'wallet.recovery.phrase.dialog.terms.and.condition.recovery',
    defaultMessage: `I understand that if this application is moved to another device or deleted, my money can 
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
    enteredPhrase: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      word: PropTypes.string.isRequired
    })).isRequired,
    isEntering: PropTypes.bool.isRequired,
    isValid: PropTypes.bool.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  actions = [
    {
      label: this.context.intl.formatMessage(messages.buttonLabel),
      onClick: this.submit
    }
  ];

  submit = () => {

  };

  onBack = () => {

  };

  onClose = () => {

  };

  render() {
    const { intl } = this.context;
    const { recoveryPhrase, enteredPhrase, isEntering, isValid } = this.props;
    const instructions = isEntering ? messages.verificationInstructions : messages.backupInstructions;
    const recoveryPhraseString = recoveryPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');
    const enteredPhraseString = enteredPhrase.reduce((phrase, { word }) => `${phrase} ${word}`, '');
    const phrase = isEntering ? enteredPhraseString : recoveryPhraseString;
    return (
      <Dialog
        title={intl.formatMessage(messages.recoveryPhrase)}
        actions={this.actions}
        active
        style={styles.component}
      >
        <WalletRecoveryInstructions instructionsText={intl.formatMessage(instructions)} />
        <WalletRecoveryPhraseMnemonic phrase={phrase} />
        {isEntering && (
          <div className={styles.words}>
            {recoveryPhrase.map(({ word }, index) => (
              <MnemonicWord
                word={word}
                key={index}
                isActive
              />
            ))}
          </div>
        )}
        <DialogCloseButton onClose={this.onClose} />
        {isEntering && (<DialogBackButton onBack={this.onBack} />)}
        {isValid && (
          <div>
            <Checkbox
              checked
              label={intl.formatMessage(messages.termDevice)}
              onChange={() => {}}
            />
            <Checkbox
              checked
              label={intl.formatMessage(messages.termRecovery)}
              onChange={() => {}}
            />
          </div>
        )}
      </Dialog>
    );
  }

}
