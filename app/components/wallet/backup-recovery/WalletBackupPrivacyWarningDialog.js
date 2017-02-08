// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import CheckboxWithLongLabel from '../../widgets/forms/CheckboxWithLongLabel';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import styles from './WalletBackupPrivacyWarningDialog.scss';

const messages = defineMessages({
  recoveryPhrase: {
    id: 'wallet.backup.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  recoveryPhraseInstructions: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions',
    defaultMessage: `!!!On the following screen, you will see a set of X random words. This is 
    your wallet backup phrase. It can be entered in any version of Daedalus application in order 
    to back up or restore your wallet’s funds and private key.`,
    description: 'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.'
  },
  buttonLabelContinue: {
    id: 'wallet.backup.privacy.warning.dialog..button.labelContinue',
    defaultMessage: '!!!Continue',
    description: 'Label for button "Continue" on wallet backup dialog'
  },
  termNobodyWatching: {
    id: 'wallet.backup.privacy.warning.dialog.checkbox.label.nobodyWatching',
    defaultMessage: '!!!Make sure nobody looks into your screen unless you want them to have access to your funds.',
    description: 'Label for the checkbox on wallet backup dialog describing that nobody should be watching when recovery phrase is shown'
  }
});

@observer
export default class WalletBackupPrivacyWarningDialog extends Component {

  static propTypes = {
    countdownRemaining: PropTypes.number.isRequired,
    canPhraseBeShown: PropTypes.bool.isRequired,
    isPrivacyNoticeAccepted: PropTypes.bool.isRequired,
    onAcceptPrivacyNotice: PropTypes.func.isRequired,
    onContinue: PropTypes.func.isRequired,
    onCancelBackup: PropTypes.func.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      countdownRemaining,
      canPhraseBeShown,
      onAcceptPrivacyNotice,
      onCancelBackup,
      isPrivacyNoticeAccepted,
      onContinue
    } = this.props;
    const countdownDisplay = countdownRemaining > 0 ? ` (${countdownRemaining})` : '';

    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabelContinue) + countdownDisplay,
        onClick: onContinue,
        disabled: !canPhraseBeShown,
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
        <WalletRecoveryInstructions
          instructionsText={intl.formatMessage(messages.recoveryPhraseInstructions)}
        />
        <CheckboxWithLongLabel
          label={intl.formatMessage(messages.termNobodyWatching)}
          onChange={onAcceptPrivacyNotice}
          checked={isPrivacyNoticeAccepted}
        />
        <DialogCloseButton onClose={onCancelBackup} />
      </Dialog>
    );
  }

}
