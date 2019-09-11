// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';

export const messages = defineMessages({
  recoveryPhraseStep4Title: {
    id: 'wallet.settings.recoveryPhraseStep4Title',
    defaultMessage: '!!!verification failure',
    description: 'Label for the recoveryPhraseStep4Title on wallet settings.',
  },
  recoveryPhraseStep4Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph1',
    defaultMessage:
      '!!!The wallet recovery phrase you have entered does not match the recovery phrase associated with this wallet. Make sure you have entered the wallet recovery phrase which was written down during the wallet creation process for this wallet and make sure the words are in the correct order.',
    description:
      'Label for the recoveryPhraseStep4Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep4Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph2',
    defaultMessage:
      '!!!If you are unable to verify your wallet recovery phrase you should create a new wallet and move all of the funds from this wallet to the new wallet. If you do this, make sure you keep the wallet recovery phrase for the new wallet safe and secure.',
    description:
      'Label for the recoveryPhraseStep4Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep4Button: {
    id: 'wallet.settings.recoveryPhraseStep4Button',
    defaultMessage: '!!!Verify recovery phrase again',
    description: 'Label for the recoveryPhraseStep4Button on wallet settings.',
  },
});

type Props = {
  onClose: Function,
  onVerifyAgain: Function,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, onVerifyAgain } = this.props;

    const actions = [
      {
        label: intl.formatMessage(messages.recoveryPhraseStep4Button),
        onClick: onVerifyAgain,
        className: 'attention',
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.recoveryPhraseStep4Title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>{intl.formatMessage(messages.recoveryPhraseStep4Paragraph1)}</p>
        <p>{intl.formatMessage(messages.recoveryPhraseStep4Paragraph2)}</p>
      </Dialog>
    );
  }
}
