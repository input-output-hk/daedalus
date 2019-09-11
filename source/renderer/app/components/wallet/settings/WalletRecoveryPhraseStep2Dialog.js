// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
// import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStep1Dialog.scss';

// export const messages = defineMessages({
//   recoveryPhraseStep1Title: {
//     id: 'wallet.settings.recoveryPhraseStep1Title',
//     defaultMessage: '!!!Wallet recovery phrase verification',
//     description: 'Label for the recoveryPhraseStep1Title on wallet settings.',
//   },
//   recoveryPhraseStep1Paragraph1: {
//     id: 'wallet.settings.recoveryPhraseStep1Paragraph1',
//     defaultMessage: '!!!To verify that you have the correct recovery phrase for this wallet you can enter your 12-word wallet recovery phrase on the following screen.',
//     description: 'Label for the recoveryPhraseStep1Paragraph1 on wallet settings.',
//   },
//     recoveryPhraseStep1Paragraph2: {
//     id: 'wallet.settings.recoveryPhraseStep1Paragraph2',
//     defaultMessage: '!!!Are you being watched? Please make sure that nobody can see your screen while you are entering your wallet recovery phrase.',
//     description: 'Label for the recoveryPhraseStep1Paragraph2 on wallet settings.',
//   },
// });

type Props = {
  onContinue: Date,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props> {
  // static contextTypes = {
  //   intl: intlShape.isRequired,
  // };
  render() {
    // const { intl } = this.context;
    const { onContinue } = this.props;
    const isSubmitting = false;

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: 'Continue',
        primary: true,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title="Wallet recovery phrase verification"
        actions={actions}
        closeOnOverlayClick
        onClose={onContinue}
        closeButton={<DialogCloseButton />}
      >
        <p>
          To verify that you have the correct recovery phrase for this wallet
          you can enter your 12-word wallet recovery phrase on the following
          screen.
        </p>
        <p>
          Are you being watched? Please make sure that nobody can see your
          screen while you are entering your wallet recovery phrase.
        </p>
      </Dialog>
    );
  }
}
