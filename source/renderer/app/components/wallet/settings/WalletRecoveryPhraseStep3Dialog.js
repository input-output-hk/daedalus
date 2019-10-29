// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';

export const messages = defineMessages({
  recoveryPhraseStep3Title: {
    id: 'wallet.settings.recoveryPhraseStep3Title',
    defaultMessage: '!!!verification successful',
    description: 'Label for the recoveryPhraseStep3Title on wallet settings.',
  },
  recoveryPhraseStep3Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep3Paragraph1',
    defaultMessage:
      '!!!You have verified the recovery phrase for this wallet. You can use it at any time to recover the funds in this wallet on another computer, even if using a different version of Daedalus.',
    description:
      'Label for the recoveryPhraseStep3Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep3Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep3Paragraph2',
    defaultMessage:
      '!!!Please make sure to return your wallet recovery phrase to a secure place for safekeeping. Anyone with access to your wallet recovery phrase can take control of your funds.',
    description:
      'Label for the recoveryPhraseStep3Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep3Button: {
    id: 'wallet.settings.recoveryPhraseStep3Button',
    defaultMessage: '!!!Continue',
    description: 'Label for the recoveryPhraseStep3Button on wallet settings.',
  },
});

type Props = {
  onClose: Function,
};

type State = {
  safetyAgreement: boolean,
};

@observer
export default class WalletRecoveryPhraseStep1 extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    safetyAgreement: false,
  };

  onToggleSafetyAgreement = (checked: boolean) => {
    this.setState({
      safetyAgreement: checked,
    });
  };

  render() {
    const { intl } = this.context;
    const { onClose } = this.props;
    const { safetyAgreement } = this.state;

    const actions = [
      {
        label: intl.formatMessage(messages.recoveryPhraseStep3Button),
        primary: true,
        onClick: onClose,
        disabled: !safetyAgreement,
      },
    ];

    const dialogStyles = classnames([styles.dialog, 'verification-successful']);

    return (
      <Dialog
        className={dialogStyles}
        title={intl.formatMessage(messages.recoveryPhraseStep3Title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p>{intl.formatMessage(messages.recoveryPhraseStep3Paragraph1)}</p>
        <p className={styles.checkboxContainer}>
          <Checkbox
            onChange={this.onToggleSafetyAgreement}
            checked={safetyAgreement}
            skin={CheckboxSkin}
            className={styles.checkbox}
            label={intl.formatMessage(messages.recoveryPhraseStep3Paragraph2)}
          />
        </p>
      </Dialog>
    );
  }
}
