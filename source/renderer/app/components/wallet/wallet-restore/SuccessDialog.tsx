import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
import tadaImage from '../../../assets/images/tada-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SuccessDialog.scss' or its c... Remove this comment to see the full error message
import styles from './SuccessDialog.scss';
import type {
  WalletDaedalusKind,
  WalletYoroiKind,
} from '../../../types/walletRestoreTypes';
import {
  WALLET_DAEDALUS_KINDS,
  WALLET_YOROI_KINDS,
} from '../../../config/walletRestoreConfig';

const messages = defineMessages({
  closeButtonLabel: {
    id: 'wallet.restore.dialog.step.success.dialog.close',
    defaultMessage: '!!!Close',
    description:
      'Label for Close button on the wallet restore "success" step dialog.',
  },
  descriptionLine1: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line1',
    defaultMessage: '!!!Your wallet has been successfully restored.',
    description:
      'Description "line 1" on the wallet restore "success" step dialog.',
  },
  descriptionLine2: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line2',
    defaultMessage:
      '!!!Restored wallets should have all the funds and transaction history of the original wallet. <strong>If your restored wallet does not have the funds and transaction history you were expecting</strong>, please check that you have the correct wallet recovery phrase for the wallet you were intending to restore.',
    description:
      'Description "line 2" on the wallet restore "success" step dialog.',
  },
  descriptionLine3: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line3',
    defaultMessage:
      '!!!<strong>If your restored wallet is empty, but you were expecting it to have funds</strong>, please check that you used the correct wallet recovery phrase during the restoration process.',
    description:
      'Description "line 3" on the wallet restore "success" step dialog.',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  walletKindDaedalus: WalletDaedalusKind | null | undefined;
  walletKindYoroi: WalletYoroiKind | null | undefined;
};
export default class SuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, walletKindDaedalus, walletKindYoroi } = this.props;
    const isDaedalusBalanceWallet =
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.BYRON_12_WORD ||
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.BYRON_27_WORD;
    const isDaedalusRewardsWallet =
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.SHELLEY_15_WORD;
    const isYoroiBalanceWallet =
      walletKindYoroi === WALLET_YOROI_KINDS.BYRON_15_WORD;
    const isYoroiRewardsWallet =
      walletKindYoroi === WALLET_YOROI_KINDS.SHELLEY_15_WORD;
    return (
      <WalletRestoreDialog
        actions={[
          {
            primary: true,
            label: intl.formatMessage(messages.closeButtonLabel),
            onClick: onClose,
          },
        ]}
        onClose={onClose}
      >
        <div className={styles.content}>
          <SVGInline svg={tadaImage} className={styles.tadaImage} />
          <div className={styles.description1}>
            <FormattedHTMLMessage {...messages.descriptionLine1} />
          </div>
          {(isDaedalusRewardsWallet || isYoroiRewardsWallet) && (
            <div className={styles.description2}>
              <FormattedHTMLMessage {...messages.descriptionLine2} />
            </div>
          )}
          {(isDaedalusBalanceWallet || isYoroiBalanceWallet) && (
            <div className={styles.description3}>
              <FormattedHTMLMessage {...messages.descriptionLine3} />
            </div>
          )}
        </div>
      </WalletRestoreDialog>
    );
  }
}
