// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import styles from './TransferFundsStep2Dialog.scss';
import { formattedWalletAmount } from '../../../utils/formatters';

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
  fees: number,
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
      fees,
      sourceWallet,
      targetWallet,
    } = this.props;

    const amount = formattedWalletAmount(sourceWallet.amount, false);
    const total = formattedWalletAmount(sourceWallet.amount.add(fees), false);

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={[
          {
            label: intl.formatMessage(messages.buttonLabel),
            onClick: onContinue,
            primary: true,
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
        <p className={styles.label}>{intl.formatMessage(messages.labelTo)}</p>
        <ul className={styles.addresses}>
          {addresses.map(address => (
            <li key={address}>{address}</li>
          ))}
        </ul>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelAmount)}
          </p>
          <div className={styles.amount}>{amount}</div>
        </div>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelFees)}
          </p>
          <div className={styles.amount}>+ {fees}</div>
        </div>
        <div className={styles.amountGroup}>
          <p className={styles.label}>
            {intl.formatMessage(messages.labelTotal)}
          </p>
          <div className={styles.amount}>{total}</div>
        </div>
      </Dialog>
    );
  }
}
