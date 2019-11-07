// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep2Dialog.scss';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.transferFunds.dialog2.title',
    defaultMessage: '!!!Transfer funds from the legacy wallet',
    description: 'Title in the transfer funds form.',
  },
  description: {
    id: 'wallet.transferFunds.dialog2.label.description',
    defaultMessage:
      '!!!Confirm transfer from the {sourceWalletName}wallet to the {targetWalletName} wallet.',
    description: 'description in the transfer funds form.',
  },
  labelTo: {
    id: 'wallet.transferFunds.dialog2.label.to',
    defaultMessage: '!!!To',
    description: 'Label To in the transfer funds form',
  },
  labelAmount: {
    id: 'wallet.transferFunds.dialog2.label.amount',
    defaultMessage: '!!!Amount',
    description: 'Label Amount in the transfer funds form',
  },
  labelFees: {
    id: 'wallet.transferFunds.dialog2.label.fees',
    defaultMessage: '!!!Fees',
    description: 'Label Fees in the transfer funds form',
  },
  labelTotal: {
    id: 'wallet.transferFunds.dialog2.label.total',
    defaultMessage: '!!!Total',
    description: 'Total Fees in the transfer funds form',
  },
  buttonLabel: {
    id: 'wallet.transferFunds.dialog2.label.buttonLabel',
    defaultMessage: '!!!Transfer funds',
    description: 'buttonLabel in the transfer funds form.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  addresses: Array<any>,
  sourceWallet: $Shape<Wallet>,
  targetWallet: $Shape<Wallet>,
  amount: string,
  fees: string,
  total: string,
};

export default class TransferFundsStep2Dialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onContinue,
      onBack,
      addresses,
      amount,
      fees,
      total,
      sourceWallet,
      targetWallet,
    } = this.props;

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={[
          {
            label: intl.formatMessage(messages.buttonLabel),
            onClick: onContinue,
          },
        ]}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <FormattedMessage
          {...messages.description}
          values={{
            sourceWalletName: <b>{sourceWallet.name}</b>,
            targetWalletName: <b>{targetWallet.name}</b>,
          }}
        >
          {(...content) => <div className={styles.description}>{content}</div>}
        </FormattedMessage>
        <p className={styles.label}>
          {intl.formatMessage(messages.labelTo)}
          {addresses.map(address => (
            <p key={address}>{address}</p>
          ))}
        </p>
        <p className={styles.label}>
          {intl.formatMessage(messages.labelAmount)}
          {amount}
        </p>
        <p className={styles.label}>
          {intl.formatMessage(messages.labelFees)}
          {fees}
        </p>
        <p className={styles.label}>
          {intl.formatMessage(messages.labelTotal)}
          {total}
        </p>
      </Dialog>
    );
  }
}
