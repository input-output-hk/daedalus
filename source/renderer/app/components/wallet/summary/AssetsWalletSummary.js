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
  handleOpenAssetSend: Function,
};

@observer
export default class AssetsWalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { wallet, assets, handleOpenAssetSend } = this.props;
    const { intl } = this.context;
    const { total } = assets;

    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = total ? total.length : null;

    return (
      numberOfAssets && (
        <Fragment>
          <div className={styles.numberOfAssets}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
          <div className={styles.component}>
            {assets.total.map((asset: any) => (
              <BorderedBox
                className={styles.assetsContainer}
                key={asset.id}
              >
                <div className={styles.assetsLeftContainer}>
                  <div className={styles.assetName}>{asset.name}</div>
                  <div className={styles.assetAmount}>
                    {isRestoreActive
                      ? '-'
                      : asset.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
                    {/* @todo Fallback for asset ticker - change it to asset name */}
                    <span>
                      &nbsp;{asset.ticker ? asset.ticker : asset.name}
                    </span>
                  </div>
                </div>
                <div className={styles.assetRightContainer}>
                  <button
                    className={classNames([
                      'primary',
                      styles.assetSendButton,
                      asset.amount.isZero() ? styles.disabled : null,
                    ])}
                    onClick={() => handleOpenAssetSend(asset)}
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
