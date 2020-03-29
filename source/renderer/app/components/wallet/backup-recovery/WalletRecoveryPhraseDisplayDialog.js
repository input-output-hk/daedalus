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
import {
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../../config/cryptoConfig';

const messages = defineMessages({
  backupInstructions: {
    id: 'wallet.backup.recovery.phrase.display.dialog.backup.instructions',
    defaultMessage:
      '!!!Please make sure you write down the {walletRecoveryPhraseWordCount} words of your wallet recovery phrase <strong>on a piece of paper in the exact order shown here</strong>.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  buttonLabelIHaveWrittenItDown: {
    id:
      'wallet.backup.recovery.phrase.display.dialog.button.label.iHaveWrittenItDown',
    defaultMessage: '!!!Yes, I have written down my wallet recovery phrase.',
    description:
      'Label for button "Yes, I have written down my wallet recovery phrase." on wallet backup dialog',
  },
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
    const { isIncentivizedTestnet } = global;
    const { recoveryPhrase, onStartWalletBackup, onCancelBackup } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'WalletRecoveryPhraseDisplayDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabelIHaveWrittenItDown),
        onClick: onStartWalletBackup,
        primary: true,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
        actions={actions}
        onClose={onCancelBackup}
        closeOnOverlayClick={false}
        closeButton={<DialogCloseButton onClose={onCancelBackup} />}
      >
        <WalletRecoveryInstructions
          instructionsText={
            <FormattedHTMLMessage
              {...messages.backupInstructions}
              values={{
                walletRecoveryPhraseWordCount: isIncentivizedTestnet
                  ? WALLET_RECOVERY_PHRASE_WORD_COUNT
                  : LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
              }}
            />
          }
        />
        <WalletRecoveryPhraseMnemonic phrase={recoveryPhrase} />
      </Dialog>
    );
  }
}
