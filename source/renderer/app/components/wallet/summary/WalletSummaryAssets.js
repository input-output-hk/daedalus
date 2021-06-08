// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryAssets.scss';
import Wallet from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletSummaryAsset from './WalletSummaryAsset';

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
  assets: Array<AssetToken>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onAssetSettings: Function,
  isLoadingAssets: boolean,
  assetSettingsDialogWasOpened: boolean,
};

type State = {
  anyAssetWasHovered: boolean,
  isSearchOpen: boolean,
};

@observer
export default class WalletSummaryAssets extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    anyAssetWasHovered: false,
    isSearchOpen: false,
  };

  handleHoverAsset = () => {
    this.setState({ anyAssetWasHovered: true });
  };

  toggleSearch = () => {
    this.setState((prevState) => ({
      isSearchOpen: !prevState.isSearchOpen,
    }));
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
    const { anyAssetWasHovered, isSearchOpen } = this.state;
    const { intl } = this.context;

    const searchButtonStyles = classNames([styles.searchButton, 'flat']);

    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets && assets.length ? assets.length : 0;

    if (isLoadingAssets) {
      return (
        <div className={styles.syncingWrapper}>
          <LoadingSpinner big />
        </div>
      );
    }

    return (
      <div className={styles.component}>
        <div className={styles.header}>
          <div className={styles.title}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
          </div>
          <Button
            className={searchButtonStyles}
            onClick={this.toggleSearch}
            label={isSearchOpen ? 'HIDE SEARCH' : 'SHOW SEARCH'}
          />
        </div>
        {isSearchOpen && (
          <BorderedBox className={styles.search}>SEARCH</BorderedBox>
        )}
        <BorderedBox>
          <div className={styles.assetsColumns}>
            <span>Token</span>
            <span>Amount</span>
          </div>
          {assets.map((asset) => (
            <WalletSummaryAsset
              key={asset.uniqueId}
              asset={asset}
              onOpenAssetSend={onOpenAssetSend}
              onCopyAssetItem={onCopyAssetItem}
              onAssetSettings={onAssetSettings}
              anyAssetWasHovered={anyAssetWasHovered}
              assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
              isLoading={isRestoreActive}
            />
          ))}
        </BorderedBox>
      </div>
    );
  }
}
