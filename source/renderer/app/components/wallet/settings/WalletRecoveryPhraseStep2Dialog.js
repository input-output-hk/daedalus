// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStep1.scss';

export const messages = defineMessages({
  recoveryPhraseValidationTitle: {
    id: 'wallet.settings.recoveryPhraseValidationTitle',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description:
      'Label for the recoveryPhraseValidationTitle on wallet settings.',
  },
  recoveryPhraseValidationDescription: {
    id: 'wallet.settings.recoveryPhraseValidationDescription',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered on another computer using the correct wallet recovery phrase. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseValidationDescription on wallet settings.',
  },
  recoveryPhraseValidationConfirmed: {
    id: 'wallet.settings.recoveryPhraseValidationConfirmed',
    defaultMessage:
      '!!!You confirmed that you still have recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseValidationConfirmed on wallet settings.',
  },
  recoveryPhraseValidationNotConfirmed: {
    id: 'wallet.settings.recoveryPhraseValidationNotConfirmed',
    defaultMessage:
      '!!!You never confirmed that you still have recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseValidationNotConfirmed on wallet settings.',
  },
  recoveryPhraseValidationNotification: {
    id: 'wallet.settings.recoveryPhraseValidationNotification',
    defaultMessage: '!!!We recommend that you check your recovery phrase.',
    description:
      'Label for the recoveryPhraseValidationNotConfirmed on wallet settings.',
  },
  recoveryPhraseValidationButton: {
    id: 'wallet.settings.recoveryPhraseValidationButton',
    defaultMessage: '!!!Confirm mnemonics.',
    description:
      'Label for the recoveryPhraseValidationButton on wallet settings.',
  },
});

type Props = {
  mnemonicsConfirmationDate: Date,
};

@observer
export default class WalletRecoveryPhraseStep2Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    // const { mnemonicsConfirmationDate } = this.props;
    const isSubmitting = false;

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.exportButtonLabel),
        primary: true,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.recoveryPhraseValidationTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={() => {}}
        closeButton={<DialogCloseButton />}
      >
        WalletRecoveryPhraseStep1
      </Dialog>
    );
  }
}
