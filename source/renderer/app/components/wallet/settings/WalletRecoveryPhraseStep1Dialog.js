// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';

export const messages = defineMessages({
  recoveryPhraseStep1Title: {
    id: 'wallet.settings.recoveryPhraseStep1Title',
    defaultMessage: '!!!Wallet recovery phrase verification',
    description: 'Label for the recoveryPhraseStep1Title on wallet settings.',
  },
  recoveryPhraseStep1Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep1Paragraph1',
    defaultMessage:
      '!!!To verify that you have the correct recovery phrase for this wallet you can enter your 12-word wallet recovery phrase on the following screen.',
    description:
      'Label for the recoveryPhraseStep1Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep1Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep1Paragraph2',
    defaultMessage:
      '!!!Are you being watched? Please make sure that nobody can see your screen while you are entering your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseStep1Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep1Button: {
    id: 'wallet.settings.recoveryPhraseStep1Button',
    defaultMessage: '!!!Continue',
    description: 'Label for the recoveryPhraseStep1Button on wallet settings.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onToggleSafetyAgreement: Function,
  safetyAgreement: boolean,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      onContinue,
      onClose,
      onToggleSafetyAgreement,
      safetyAgreement,
    } = this.props;
    const isSubmitting = false;

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.recoveryPhraseStep1Button),
        primary: true,
        onClick: onContinue,
        disabled: !safetyAgreement,
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.recoveryPhraseStep1Title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>{intl.formatMessage(messages.recoveryPhraseStep1Paragraph1)}</p>
        <p className={styles.checkboxContainer}>
          <Checkbox
            onChange={onToggleSafetyAgreement}
            checked={safetyAgreement}
            skin={CheckboxSkin}
            className={styles.checkbox}
          />
          {intl.formatMessage(messages.recoveryPhraseStep1Paragraph2)}
        </p>
      </Dialog>
    );
  }
}
