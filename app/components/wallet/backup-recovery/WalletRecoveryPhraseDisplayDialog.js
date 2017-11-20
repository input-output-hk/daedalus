// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletRecoveryPhraseDisplayDialog.scss';

const messages = defineMessages({
  backupInstructions: {
    id: 'wallet.backup.recovery.phrase.display.dialog.backup.instructions',
    defaultMessage: `!!!Please, make sure you have carefully written down your recovery phrase somewhere safe.
    You will need this phrase later for next use and recover. Phrase is case sensitive.`,
    description: 'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.'
  },
  buttonLabelIHaveWrittenItDown: {
    id: 'wallet.backup.recovery.phrase.display.dialog.button.label.iHaveWrittenItDown',
    defaultMessage: '!!!Yes, I’ve written it down',
    description: 'Label for button "Yes, I’ve written it down" on wallet backup dialog'
  }
});

type Props = {
  recoveryPhrase: string,
  onStartWalletBackup: Function,
  onCancelBackup: Function,
};

@observer
export default class WalletRecoveryPhraseDisplayDialog extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      recoveryPhrase,
      onStartWalletBackup,
      onCancelBackup,
    } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'WalletRecoveryPhraseDisplayDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabelIHaveWrittenItDown),
        onClick: onStartWalletBackup,
        primary: true
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
        actions={actions}
        onClose={onCancelBackup}
        closeOnOverlayClick
        closeButton={<DialogCloseButton onClose={onCancelBackup} />}
      >
        <WalletRecoveryInstructions
          instructionsText={<FormattedHTMLMessage {...messages.backupInstructions} />}
        />
        <WalletRecoveryPhraseMnemonic phrase={recoveryPhrase} />
      </Dialog>
    );
  }

}
