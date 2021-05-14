// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import classNames from 'classnames';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryAssets.scss';
import Wallet from '../../../domains/Wallet';
import AssetToken from '../../assets/AssetToken';
import AssetAmount from '../../assets/AssetAmount';
import type { WalletSummaryAsset } from '../../../api/assets/types';
import LoadingSpinner from '../../widgets/LoadingSpinner';

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
  onAssetSettings: Function,
  isLoadingAssets: boolean,
  assetSettingsDialogWasOpened: boolean,
};

type State = {
  anyAssetWasHovered: boolean,
};

@observer
export default class WalletSummaryAssets extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    anyAssetWasHovered: false,
  };

  handleHoverAsset = () => {
    this.setState({ anyAssetWasHovered: true });
  };

  render() {
    const {
      wallet,
      assets,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
    } = this.props;
    const { anyAssetWasHovered } = this.state;
    const { intl } = this.context;

    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets && assets.length ? assets.length : 0;

    return (
      <Fragment>
        {!isLoadingAssets && (
          <div className={styles.numberOfAssets}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
        )}
        {/* eslint-disable-next-line no-nested-ternary */}
        {isLoadingAssets ? (
          <div className={styles.syncingWrapper}>
            <LoadingSpinner big />
          </div>
        ) : (
          <div className={styles.component}>
            {assets.map((asset: WalletSummaryAsset, index: number) => (
              <BorderedBox
                className={styles.assetsContainer}
                key={asset.policyId + asset.assetName + asset.fingerprint}
                onMouseEnter={this.handleHoverAsset}
              >
                {asset.fingerprint && (
                  <div className={styles.assetsLeftContainer}>
                    <AssetToken
                      asset={asset}
                      onCopyAssetItem={onCopyAssetItem}
                      metadataNameChars={get('name', asset.metadata, 0)}
                      onClickSettings={() => onAssetSettings({ asset })}
                      assetSettingsDialogWasOpened={
                        index === 0 ? assetSettingsDialogWasOpened : null
                      }
                      anyAssetWasHovered={anyAssetWasHovered}
                    />
                    <div>
                      <AssetAmount
                        amount={asset.quantity}
                        metadata={asset.metadata}
                        decimals={asset.decimals}
                        isLoading={isRestoreActive}
                        className={styles.assetAmount}
                      />
                    </div>
                  </div>
                )}
                {asset.fingerprint && (
                  <div className={styles.assetRightContainer}>
                    <button
                      className={classNames([
                        'primary',
                        styles.assetSendButton,
                        asset.quantity.isZero() ? styles.disabled : null,
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
        )}
      </Fragment>
    );
  }
}
