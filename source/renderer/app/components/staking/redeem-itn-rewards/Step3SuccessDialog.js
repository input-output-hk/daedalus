// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './Step3SuccessDialog.scss';
import redeemDialogOverride from './RedeemDialogOverride.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';
import tadaImage from '../../../assets/images/tada-ic.inline.svg';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step3.success.title',
    defaultMessage: '!!!Incentivized Testnet rewards redeemed!',
    description: 'title for Redeem Incentivized Testnet - Step 3',
  },
  description: {
    id: 'staking.redeemItnRewards.step3.success.description',
    defaultMessage:
      '!!!You have successfully redeemed <b>{finalTotal}</b> ADA to your <b>{walletName}</b> wallet. This transaction incurred <b>{transactionFees}</b> in transaction fees',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  openWalletButtonLabel: {
    id: 'staking.redeemItnRewards.step3.success.openWalletButtonLabel',
    defaultMessage: '!!!Open the wallet',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  downloadPDFButtonLabel: {
    id: 'staking.redeemItnRewards.step3.success.downloadPDFButtonLabel',
    defaultMessage: '!!!Download PDF certificate',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
});

type Props = {
  wallet: Wallet,
  transactionFees: number,
  finalTotal: number,
  onContinue: Function,
  onClose: Function,
  onPDFDownload: Function,
};

@observer
export default class Step3SuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      wallet,
      transactionFees,
      finalTotal,
      onContinue,
      onPDFDownload,
      onClose,
    } = this.props;

    const { name: walletName } = wallet;

    return (
      <Dialog
        onClose={onClose}
        actions={[
          {
            primary: true,
            label: intl.formatMessage(messages.openWalletButtonLabel),
            onClick: onContinue,
          },
          {
            primary: true,
            label: intl.formatMessage(messages.downloadPDFButtonLabel),
            onClick: onPDFDownload,
          },
        ]}
        closeButton={<DialogCloseButton />}
        customThemeOverrides={redeemDialogOverride}
        closeOnOverlayClick={false}
      >
        <div className={styles.title}>{intl.formatMessage(messages.title)}</div>
        <SVGInline svg={tadaImage} className={styles.tadaImage} />
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              walletName,
              transactionFees: formattedWalletAmount(transactionFees),
              finalTotal: formattedWalletAmount(finalTotal),
            }}
          />
        </div>
      </Dialog>
    );
  }
}
