// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import ledgerIcon from '../../../assets/images/hardware-wallet/ledger-cropped.inline.svg';
import trezorIcon from '../../../assets/images/hardware-wallet/trezor-ledger.inline.svg';
import exportIcon from '../../../assets/images/hardware-wallet/export.inline.svg';
import checkIcon from '../../../assets/images/hardware-wallet/check.inline.svg';
import styles from './ConnectHardwareWallet.scss';

const messages = defineMessages({
  hardwareWalletTitle: {
    id: 'wallet.hardware.hardwareWalletTitle',
    defaultMessage: '!!!Hardware wallet',
    description: 'Hardware wallet title.',
  },
  ledgerWalletTitle: {
    id: 'wallet.hardware.ledgerWalletTitle',
    defaultMessage: '!!!Ledger wallet',
    description: 'Ledger wallet title.',
  },
  hardwareWalletInstructions: {
    id: 'wallet.hardware.hardwareWalletInstructions',
    defaultMessage: '!!!Follow instructions to access your wallet',
    description: 'Follow instructions label',
  },
  hardwareWalletLedgerBegin: {
    id: 'wallet.hardware.hardwareWalletLedgerBegin',
    defaultMessage:
      '!!!To begin, connect and unlock your <span>Ledger Device</span>',
    description: 'Connect device label',
  },
  hardwareWalletBegin: {
    id: 'wallet.hardware.hardwareWalletBegin',
    defaultMessage:
      '!!!To begin, connect and unlock your <span>Hardware wallet Device</span>',
    description: 'Connect device label',
  },
  hardwareWalletExport: {
    id: 'wallet.hardware.hardwareWalletExport',
    defaultMessage: '!!!Export <span>public key</span> on your device',
    description: 'Export wallet label',
  },
});

type Props = {
  onOpenExternalLink: Function,
};

@observer
export default class ConnectHardwareWallet extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const { onOpenExternalLink } = this.props;

    return (
      <>
        <div className={styles.component}>
          <div className={styles.hardwareWalletContainer}>
            <div className={styles.hardwareWalletWrapper}>
              <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
              <h2 className={styles.hardwareWalletTitle}>
                {intl.formatMessage(messages.hardwareWalletTitle)}
              </h2>
              <p className={styles.hardwareWalletMessage}>
                {intl.formatMessage(messages.hardwareWalletInstructions)}
              </p>
              <div className={styles.hardwareWalletStepsWrapper}>
                <div className={styles.hardwareWalletStep}>
                  {intl.formatMessage(messages.hardwareWalletBegin)}
                </div>
                <div className={styles.hardwareWalletStep}>
                  {intl.formatMessage(messages.hardwareWalletExport)}
                </div>
              </div>
            </div>
          </div>
        </div>
      </>
    );
  }
}
