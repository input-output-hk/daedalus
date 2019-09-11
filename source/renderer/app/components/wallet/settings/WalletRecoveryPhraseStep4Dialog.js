// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
// import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';

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
  onClose: Function,
  onVerifyAgain: Function,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props> {
  // static contextTypes = {
  //   intl: intlShape.isRequired,
  // };
  render() {
    // const { intl } = this.context;
    const { onClose, onVerifyAgain } = this.props;

    const actions = [
      {
        label: 'Verify recovery phrase again',
        onClick: onVerifyAgain,
        className: 'attention',
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title="verification failure"
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>
          The wallet recovery phrase you have entered does not match the
          recovery phrase associated with this wallet. Make sure you have
          entered the wallet recovery phrase which was written down during the
          wallet creation process for this wallet and make sure the words are in
          the correct order.
        </p>
        <p>
          If you are unable to verify your wallet recovery phrase you should
          create a new wallet and move all of the funds from this wallet to the
          new wallet. If you do this, make sure you keep the wallet recovery
          phrase for the new wallet safe and secure.
        </p>
      </Dialog>
    );
  }
}
