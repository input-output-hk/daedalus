// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';

export const messages = defineMessages({
  recoveryPhraseStep2Title: {
    id: 'wallet.settings.recoveryPhraseStep2Title',
    defaultMessage: '!!!Wallet recovery phrase verification',
    description: 'Label for the recoveryPhraseStep2Title on wallet settings.',
  },
  recoveryPhraseStep2Description: {
    id: 'wallet.settings.recoveryPhraseStep2Description',
    defaultMessage:
      '!!!Please enter your 12-word wallet recovery phrase. Make sure you enter the words in the correct order.',
    description:
      'Label for the recoveryPhraseStep2Description on wallet settings.',
  },
  recoveryPhraseStep2Subtitle: {
    id: 'wallet.settings.recoveryPhraseStep2Subtitle',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recoveryPhraseStep2Subtitle on wallet settings.',
  },
  recoveryPhraseStep2Button: {
    id: 'wallet.settings.recoveryPhraseStep2Button',
    defaultMessage: '!!!Verify',
    description: 'Label for the recoveryPhraseStep2Button on wallet settings.',
  },
});

type Props = {
  onVerify: Function,
  onClose: Function,
};

type State = {
  isVeryfying: boolean,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isVeryfying: false,
  };

  handleVerify = () => {
    this.setState({
      isVeryfying: true,
    });
    this.props.onVerify();
  };

  render() {
    const { intl } = this.context;
    const { onClose, onVerify } = this.props;
    const { isVeryfying } = this.state;

    const actions = [
      {
        className: isVeryfying ? styles.isVeryfying : null,
        label: `${intl.formatMessage(
          messages.recoveryPhraseStep2Button
        )} - successfuly`,
        primary: true,
        onClick: () => onVerify(true),
        disabled: isVeryfying,
      },
      {
        label: `${intl.formatMessage(
          messages.recoveryPhraseStep2Button
        )} - failure`,
        onClick: () => onVerify(false),
        className: 'attention',
        disabled: isVeryfying,
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.recoveryPhraseStep2Title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>{intl.formatMessage(messages.recoveryPhraseStep2Description)}</p>
        <div className={styles.subtitle}>
          <h2>{intl.formatMessage(messages.recoveryPhraseStep2Subtitle)}</h2>
        </div>
      </Dialog>
    );
  }
}
