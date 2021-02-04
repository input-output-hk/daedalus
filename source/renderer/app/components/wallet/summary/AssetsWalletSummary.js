// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import BorderedBox from '../../widgets/BorderedBox';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './AssetsWalletSummary.scss';
import Wallet from '../../../domains/Wallet';
import type { WalletAssets } from '../../../api/assets/types';

const messages = defineMessages({
  transactionsLabel: {
    id: 'wallet.summary.page.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary page',
  },
  pendingTransactionsLabel: {
    id: 'wallet.summary.page.pendingTransactionsLabel',
    defaultMessage: '!!!Number of pending transactions',
    description:
      '"Number of pending transactions" label on Wallet summary page',
  },
  tokensTitle: {
    id: 'wallet.summary.page.tokensTitle',
    defaultMessage: '!!!Assets',
    description: 'Number of assets title on Wallet summary page',
  },
  tokenSendButton: {
    id: 'wallet.summary.page.tokenSendButton',
    defaultMessage: '!!!Send',
    description: 'Send button on Wallet summary page',
  },
});

type Props = {
  wallet: Wallet,
  assets: WalletAssets,
  handleOpenWalletTokenSend: Function,
};

@observer
export default class AssetsWalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { wallet, assets, handleOpenWalletTokenSend } = this.props;
    const { intl } = this.context;
    const { total } = assets;

    const isRestoreActive = wallet.isRestoring;
    const numberOfNativeTokens = total ? total.length : null;

    return (
      numberOfNativeTokens && (
        <Fragment>
          <div className={styles.numberOfTokens}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfNativeTokens})
          </div>
          <div className={styles.component}>
            {assets.total.map((token: any) => (
              <BorderedBox
                className={styles.nativeTokenContainer}
                key={token.id}
              >
                <div className={styles.nativeTokenLeftContainer}>
                  <div className={styles.walletName}>{token.name}</div>
                  <div className={styles.walletAmount}>
                    {isRestoreActive
                      ? '-'
                      : token.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
                    {/* @todo Fallback for token ticker - change it to token name */}
                    <span>
                      &nbsp;{token.ticker ? token.ticker : token.name}
                    </span>
                  </div>
                </div>
                <div className={styles.nativeTokenRightContainer}>
                  <button
                    className={classNames([
                      'primary',
                      styles.nativeTokenSendButton,
                      token.amount.isZero() ? styles.disabled : null,
                    ])}
                    onClick={() => handleOpenWalletTokenSend(token)}
                  >
                    {intl.formatMessage(messages.tokenSendButton)}
                  </button>
                </div>
              </BorderedBox>
            ))}
          </div>
        </Fragment>
      )
    );
  }
}
