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
    defaultMessage: '!!!Transfer funds from the legacy wallet',
    description: 'Title  in the transfer funds form.',
  },
  fromWallet: {
    id: 'wallet.transferFunds.dialog1.fromWallet',
    defaultMessage: '!!!From wallet',
    description: 'fromWallet in the transfer funds form.',
  },
  toWallet: {
    id: 'wallet.transferFunds.dialog1.toWallet',
    defaultMessage: '!!!To walet',
    description: 'toWallet in the transfer funds form.',
  },
  continue: {
    id: 'global.dialog.button.continue',
    defaultMessage: '!!!Continue',
    description: 'continue in the transfer funds form.',
  },
});

type Props = {
  onClose: Function,
  onContinue: Function,
  onSetToWallet: Function,
  walletToId?: string,
  walletFrom: $Shape<Wallet>,
  wallets: Array<Wallet>,
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
      onSetToWallet,
      walletToId,
      walletFrom,
      wallets,
    } = this.props;

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={[
          {
            label: intl.formatMessage(messages.continue),
            onClick: onContinue,
          },
        ]}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <h3>{intl.formatMessage(messages.fromWallet)}</h3>
        <div className={styles.walletFrom}>
          <WalletsDropdownOption
            label={walletFrom.name}
            detail={formattedWalletAmount(walletFrom.amount)}
            selected
          />
        </div>
        <WalletsDropdown
          label={intl.formatMessage(messages.toWallet)}
          wallets={wallets}
          onChange={onSetToWallet}
          value={walletToId}
        />
      </Dialog>
    );
  }
}
