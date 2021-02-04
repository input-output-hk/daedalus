// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './AssetsWalletSummary.scss';
import Wallet from '../../../domains/Wallet';
import type { AssetMetadata, WalletAssetItems } from '../../../api/assets/types';

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

type WalletSummaryAsset = {
  id: string,
  metadata: AssetMetadata,
  total: WalletAssetItems,
};

type Props = {
  wallet: Wallet,
  assets: Array<WalletSummaryAsset>,
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

    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets ? assets.length : null;

    return (
      numberOfAssets && (
        <Fragment>
          <div className={styles.numberOfAssets}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
          <div className={styles.component}>
            {assets.map((asset: WalletSummaryAsset) => (
              <BorderedBox
                className={styles.assetsContainer}
                key={asset.id}
              >
                <div className={styles.assetsLeftContainer}>
                  <div className={styles.assetName}>{asset.metadata.name}</div>
                  <div className={styles.assetAmount}>
                    {isRestoreActive
                      ? '-'
                      : new BigNumber(asset.total.quantity).toFormat(asset.metadata.unit.decimals)}
                    <span>
                      &nbsp;{asset.metadata.acronym}
                    </span>
                  </div>
                </div>
                <div className={styles.assetRightContainer}>
                  <button
                    className={classNames([
                      'primary',
                      styles.assetSendButton,
                      new BigNumber(asset.total.quantity).isZero() ? styles.disabled : null,
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
