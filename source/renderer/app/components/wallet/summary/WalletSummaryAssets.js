// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryAssets.scss';
import Wallet from '../../../domains/Wallet';
import AssetToken from '../../widgets/AssetToken';
import type { WalletSummaryAsset } from '../../../api/assets/types';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletSummaryNoTokens from './WalletSummaryNoTokens';

const messages = defineMessages({
  tokensTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Number of tokens title on Wallet summary assets page',
  },
  tokenSendButton: {
    id: 'wallet.summary.assets.tokenSendButton',
    defaultMessage: '!!!Send',
    description: 'Send button on Wallet summary assets page',
  },
  unknownLabel: {
    id: 'wallet.summary.assets.unknownLabel',
    defaultMessage: '!!!Unknown',
    description: 'Unknown label on Wallet summary assets page',
  },
});

type Props = {
  wallet: Wallet,
  assets: Array<WalletSummaryAsset>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onExternalLinkClick: Function,
  isLoading?: boolean,
};

@observer
export default class WalletSummaryAssets extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      assets,
      onOpenAssetSend,
      onCopyAssetItem,
      onExternalLinkClick,
      isLoading,
    } = this.props;
    const { intl } = this.context;

    const { isHardwareWallet } = wallet;
    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets && assets.length ? assets.length : 0;

    return (
      <Fragment>
        {!isLoading && (
          <div className={styles.numberOfAssets}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
        )}
        {/* eslint-disable-next-line no-nested-ternary */}
        {isLoading ? (
          <div className={styles.syncingWrapper}>
            <LoadingSpinner big />
          </div>
        ) : numberOfAssets ? (
          <div className={styles.component}>
            {assets.map((asset: WalletSummaryAsset, index: number) => (
              <BorderedBox
                className={styles.assetsContainer}
                // @TOKEN TODO: Remove once we have the correct data being returned
                // eslint-disable-next-line react/no-array-index-key
                key={asset.policyId + asset.fingerprint + index}
              >
                {asset.fingerprint && (
                  <div className={styles.assetsLeftContainer}>
                    <AssetToken
                      asset={asset}
                      onCopyAssetItem={onCopyAssetItem}
                      metadataNameChars={get('name', asset.metadata, 0)}
                    />
                    <div className={styles.assetAmount}>
                      {isRestoreActive
                        ? '-'
                        : new BigNumber(asset.quantity).toFormat(
                            asset.metadata && asset.metadata.unit
                              ? asset.metadata.unit.decimals
                              : DECIMAL_PLACES_IN_ADA
                          )}
                      {asset.metadata && (
                        <span>&nbsp;{asset.metadata.acronym}</span>
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
                        new BigNumber(asset.quantity).isZero() ||
                        isHardwareWallet
                          ? styles.disabled
                          : null,
                      ])}
                      onClick={() => onOpenAssetSend(asset)}
                    >
                      {intl.formatMessage(messages.tokenSendButton)}
                    </button>
                  </div>
                )}
              </BorderedBox>
            ))}
          </div>
        ) : (
          <WalletSummaryNoTokens onExternalLinkClick={onExternalLinkClick} />
        )}
      </Fragment>
    );
  }
}
