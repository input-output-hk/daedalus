// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { get } from 'lodash';
import styles from './WalletSummaryAssets.scss';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import type { AssetToken } from '../../../api/assets/types';

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
  asset: AssetToken,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onAssetSettings: Function,
  anyAssetWasHovered: boolean,
  isLoading: boolean,
  assetSettingsDialogWasOpened: boolean,
};

type State = {
  isExpanded: boolean,
};

@observer
export default class WalletSummaryAsset extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isExpanded: false,
  };

  render() {
    const { intl } = this.context;
    const {
      asset,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      isLoading,
      anyAssetWasHovered,
      assetSettingsDialogWasOpened,
    } = this.props;
    const { isExpanded } = this.state;
    const componentStyles = classNames(styles.component, {
      [styles.isExpanded]: isExpanded,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.assetsLeftContainer}>
          <Asset
            asset={asset}
            onCopyAssetItem={onCopyAssetItem}
            metadataNameChars={get('name', asset.metadata, 0)}
            onClickSettings={() => onAssetSettings({ asset })}
            assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
            anyAssetWasHovered={anyAssetWasHovered}
          />
          <div>
            <AssetAmount
              amount={asset.quantity}
              metadata={asset.metadata}
              decimals={asset.decimals}
              isLoading={isLoading}
              className={styles.assetAmount}
            />
          </div>
        </div>
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
      </div>
    );
  }
}
