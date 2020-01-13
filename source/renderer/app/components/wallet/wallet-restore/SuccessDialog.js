// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import tadaImage from '../../../assets/images/tada-ic.inline.svg';
import styles from './SuccessDialog.scss';
import type {
  WalletDaedalusKind,
  WalletKind,
  WalletYoroiKind,
} from '../../../types/walletRestoreTypes';
import {
  WALLET_DAEDALUS_KINDS,
  WALLET_KINDS,
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
      '!!!<strong>If your restored Balance wallet is empty, but you were expecting it to have funds</strong>, please check that you used the correct wallet recovery phrase during the restoration process.',
    description:
      'Description "line 3" on the wallet restore "success" step dialog.',
  },
  descriptionLine4: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line4',
    defaultMessage:
      '!!!If your restored Balance wallet is empty, but you were expecting it to have funds, please check that you used the correct wallet recovery phrase during the restoration process. <strong>The format of recovery phrases for paper wallets cannot be validated</strong>, so any combination of words is accepted as a potentially valid recovery phrase. Please take extra care when entering a paper wallet recovery phrase.',
    description:
      'Description "line 4" on the wallet restore "success" step dialog.',
  },
  descriptionLine5: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line5',
    defaultMessage:
      '!!!To participate in the Incentivized Testnet, the mainnet wallet you are restoring must have had funds at the time of the balance snapshot at 12.00 UTC, November 29, 2019. If you are sure that you used the correct wallet recovery phrase, then please check that you had funds in your mainnet wallet at this time.',
    description:
      'Description "line 5" on the wallet restore "success" step dialog.',
  },
});

type Props = {
  onClose: Function,
  walletKind: ?WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
};

export default class SuccessDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
    } = this.props;

    const isPaperWallet = walletKind === WALLET_KINDS.HARDWARE;
    const isDaedalusBalanceWallet =
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.BALANCE_12_WORD ||
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.BALANCE_27_WORD;
    const isDaedalusRewardsWallet =
      walletKindDaedalus === WALLET_DAEDALUS_KINDS.REWARD_15_WORD;
    const isYoroiBalanceWallet =
      walletKindYoroi === WALLET_YOROI_KINDS.BALANCE_15_WORD;
    const isYoroiRewardsWallet =
      walletKindYoroi === WALLET_YOROI_KINDS.REWARD_15_WORD;

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
          {isPaperWallet && (
            <div className={styles.description4}>
              <FormattedHTMLMessage {...messages.descriptionLine4} />
            </div>
          )}
          {(isDaedalusBalanceWallet ||
            isYoroiBalanceWallet ||
            isPaperWallet) && (
            <div className={styles.description5}>
              <FormattedHTMLMessage {...messages.descriptionLine5} />
            </div>
          )}
        </div>
      </WalletRestoreDialog>
    );
  }
}
