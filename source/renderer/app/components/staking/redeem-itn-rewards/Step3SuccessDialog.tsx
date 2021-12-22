import React, { Component } from 'react';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Step3SuccessDialog.scss' or ... Remove this comment to see the full error message
import styles from './Step3SuccessDialog.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
import tadaImage from '../../../assets/images/tada-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step3.success.title',
    defaultMessage: '!!!Incentivized Testnet rewards redeemed!',
    description: 'title for Redeem Incentivized Testnet - Step 3',
  },
  description: {
    id: 'staking.redeemItnRewards.step3.success.description',
    defaultMessage:
      '!!!You have successfully redeemed <b>{redeemedRewards}</b> to your <b>{walletName}</b> wallet. This transaction incurred <b>{transactionFees}</b> in transaction fees',
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
  wallet: Wallet;
  transactionFees: BigNumber;
  redeemedRewards: BigNumber;
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onPDFDownload?: (...args: Array<any>) => any;
};

@observer
class Step3SuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      wallet,
      transactionFees,
      redeemedRewards,
      onContinue,
      onPDFDownload,
      onClose,
    } = this.props;
    const { name: walletName } = wallet;
    const actions = [
      {
        primary: true,
        label: intl.formatMessage(messages.openWalletButtonLabel),
        onClick: onContinue,
      },
    ];
    if (onPDFDownload)
      actions.push({
        primary: true,
        label: intl.formatMessage(messages.downloadPDFButtonLabel),
        onClick: onPDFDownload,
      });
    const closeButton = (
      <DialogCloseButton
        icon={closeCrossThin}
        className={styles.closeButton}
        onClose={onClose}
      />
    );
    return (
      <Dialog
        onClose={onClose}
        actions={actions}
        closeButton={closeButton}
        closeOnOverlayClick={false}
        fullSize
      >
        <div className={styles.title}>{intl.formatMessage(messages.title)}</div>
        <SVGInline svg={tadaImage} className={styles.tadaImage} />
        <div className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              walletName,
              transactionFees: formattedWalletAmount(transactionFees),
              redeemedRewards: formattedWalletAmount(redeemedRewards),
            }}
          />
        </div>
      </Dialog>
    );
  }
}

export default Step3SuccessDialog;
