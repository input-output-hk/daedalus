// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './AssetsWalletSummary.scss';
import Wallet from '../../../domains/Wallet';
import AssetToken from '../../widgets/AssetToken';
import type { WalletSummaryAsset } from '../../../api/assets/types';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import LoadingSpinner from '../../widgets/LoadingSpinner';

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
    defaultMessage: '!!!Tokens',
    description: 'Number of tokens title on Wallet summary page',
  },
  tokenSendButton: {
    id: 'wallet.summary.page.tokenSendButton',
    defaultMessage: '!!!Send',
    description: 'Send button on Wallet summary page',
  },
  unknownLabel: {
    id: 'wallet.summary.page.unknownLabel',
    defaultMessage: '!!!Unknown',
    description: 'Unknown label on Wallet summary page',
  },
});

type Props = {
  wallet: Wallet,
  assets: Array<WalletSummaryAsset>,
  handleOpenAssetSend: Function,
  isLoading?: boolean,
};

@observer
export default class AssetsWalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { wallet, assets, handleOpenAssetSend, isLoading } = this.props;
    const { intl } = this.context;

    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets ? assets.length : null;

    return (
      numberOfAssets && (
        <Fragment>
          {!isLoading && (
            <div className={styles.numberOfAssets}>
              {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
            </div>
          )}
          {isLoading ? (
            <div className={styles.syncingWrapper}>
              <LoadingSpinner />
            </div>
          ) : (
            <div className={styles.component}>
              {assets.map((asset: WalletSummaryAsset) => (
                <BorderedBox
                  className={styles.assetsContainer}
                  key={asset.fingerprint}
                >
                  {asset.fingerprint && (
                    <div className={styles.assetsLeftContainer}>
                      <AssetToken asset={asset} />
                      <div className={styles.assetAmount}>
                        {isRestoreActive
                          ? '-'
                          : new BigNumber(asset.quantity).toFormat(
                              asset.metadata && asset.metadata.unit
                                ? asset.metadata.unit.decimals
                                : DECIMAL_PLACES_IN_ADA
                            )}
                        {asset.metadata ? (
                          <span>&nbsp;{asset.metadata.acronym}</span>
                        ) : (
                          <span>
                            &nbsp;
                            {intl
                              .formatMessage(messages.unknownLabel)
                              .toString()
                              .substr(0, 3)}
                          </span>
                        )}
                      </div>
                    </div>
                  )}
                  {asset.fingerprint && (
                    <div className={styles.assetRightContainer}>
                      <button
                        className={classNames([
                          'primary',
                          styles.assetSendButton,
                          new BigNumber(asset.quantity).isZero()
                            ? styles.disabled
                            : null,
                        ])}
                        onClick={() => handleOpenAssetSend(asset)}
                      >
                        {intl.formatMessage(messages.tokenSendButton)}
                      </button>
                    </div>
                  )}
                </BorderedBox>
              ))}
            </div>
          )}
        </Fragment>
      )
    );
  }
}
