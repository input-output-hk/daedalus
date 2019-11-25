// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep1Dialog.scss';
import Wallet from '../../../domains/Wallet';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import WalletsDropdownOption from '../../widgets/forms/WalletsDropdownOption';
import { formattedWalletAmount } from '../../../utils/formatters';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog1.title',
    defaultMessage: '!!!Transfer funds from the Balance wallet',
    description: 'Title  in the transfer funds form.',
  },
  sourceWallet: {
    id: 'wallet.transferFunds.dialog1.sourceWallet',
    defaultMessage: '!!!From Balance wallet',
    description: 'sourceWallet in the transfer funds form.',
  },
  targetWallet: {
    id: 'wallet.transferFunds.dialog1.targetWallet',
    defaultMessage: '!!!To Rewards wallet',
    description: 'targetWallet in the transfer funds form.',
  },
  buttonLabel: {
    id: 'wallet.transferFunds.dialog1.continueLabel',
    defaultMessage: '!!!Continue',
    description: 'buttonLabel in the transfer funds form.',
  },
});

type Props = {
  onClose: Function,
  onContinue: Function,
  onSetSourceWallet: Function,
  targetWalletId: string,
  sourceWallet: $Shape<Wallet>,
  wallets: Array<$Shape<Wallet>>,
};

export default class TransferFundsStep1Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue,
      onSetSourceWallet,
      targetWalletId,
      sourceWallet,
      wallets,
    } = this.props;

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={[
          {
            label: intl.formatMessage(messages.buttonLabel),
            onClick: onContinue,
            primary: true,
            disabled: !this.props.targetWalletId,
          },
        ]}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <p className={styles.label}>
          {intl.formatMessage(messages.sourceWallet)}
        </p>
        <div className={styles.sourceWallet}>
          <WalletsDropdownOption
            label={sourceWallet.name}
            detail={formattedWalletAmount(sourceWallet.amount)}
            selected
          />
        </div>
        <WalletsDropdown
          label={intl.formatMessage(messages.targetWallet)}
          wallets={wallets}
          onChange={onSetSourceWallet}
          value={targetWalletId}
        />
      </Dialog>
    );
  }
}
