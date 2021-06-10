// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import SVGInline from 'react-svg-inline';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryAssets.scss';
import Wallet from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletSummaryAsset from './WalletSummaryAsset';
import { onSearchAssetsDropdown } from '../../widgets/forms/AssetsDropdown';
import searchIcon from '../../../assets/images/search.inline.svg';
import crossIcon from '../../../assets/images/close-cross.inline.svg';

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
  hideSearchButtonLabel: {
    id: 'wallet.summary.assets.search.button.label.hide',
    defaultMessage: '!!!Hide search',
    description: 'Hide search label on Wallet summary assets page',
  },
  showSearchButtonLabel: {
    id: 'wallet.summary.assets.search.button.label.show',
    defaultMessage: '!!!Show search',
    description: 'Show search label on Wallet summary assets page',
  },
  searchInputPlaceholder: {
    id: 'wallet.summary.assets.search.input.placeholder',
    defaultMessage: '!!!Filter token list',
    description: 'Search placeholder on Wallet summary assets page',
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
  searchValue: string,
};

@observer
export default class WalletSummaryAssets extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    anyAssetWasHovered: false,
    isSearchOpen: false,
    searchValue: '',
  };

  handleHoverAsset = () => {
    this.setState({ anyAssetWasHovered: true });
  };

  toggleSearch = () => {
    this.setState((prevState) => ({
      isSearchOpen: !prevState.isSearchOpen,
    }));
  };

  get filteredAssets() {
    const { searchValue } = this.state;
    const { assets } = this.props;
    return onSearchAssetsDropdown(
      searchValue,
      assets.map((asset) => ({ asset }))
    );
  }

  setSearchValue = (searchValue: string) => {
    this.setState({
      searchValue,
    });
  };

  render() {
    const {
      wallet,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
    } = this.props;
    const { anyAssetWasHovered, isSearchOpen, searchValue } = this.state;
    const { intl } = this.context;
    const { filteredAssets } = this;
    const assets = filteredAssets.map(({ asset }) => asset);
    const searchButtonStyles = classNames([styles.searchButton, 'flat']);
    const isRestoreActive = wallet.isRestoring;
    const numberOfAssets = assets && assets.length ? assets.length : 0;
    const noResults = !assets.length && searchValue.length >= 3;

    if (isLoadingAssets) {
      return (
        <div className={styles.syncingWrapper}>
          <LoadingSpinner big />
        </div>
      );
    }

    const searchButtonLabel = isSearchOpen
      ? intl.formatMessage(messages.hideSearchButtonLabel)
      : intl.formatMessage(messages.showSearchButtonLabel);

    return (
      <div className={styles.component}>
        <div className={styles.header}>
          <div className={styles.title}>
            {intl.formatMessage(messages.tokensTitle)} ({numberOfAssets})
            {!!searchValue.length && (
              <>
                &nbsp;-&nbsp;
                <button
                  className={styles.searchValue}
                  onClick={() => this.setSearchValue('')}
                >
                  {searchValue} <SVGInline svg={crossIcon} />
                </button>
              </>
            )}
          </div>
          <Button
            className={searchButtonStyles}
            onClick={this.toggleSearch}
            label={searchButtonLabel}
          />
        </div>
        {isSearchOpen && (
          <div className={styles.search}>
            <SVGInline svg={searchIcon} className={styles.searchIcon} />
            <Input
              className={styles.spendingPassword}
              onChange={this.setSearchValue}
              value={searchValue}
              placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
            />
            {!!searchValue.length && (
              <button
                className={classNames([styles.clearButton, 'flat'])}
                onClick={() => this.setSearchValue('')}
              >
                <SVGInline svg={crossIcon} />
              </button>
            )}
          </div>
        )}
        <BorderedBox>
          <div className={styles.assetsColumns}>
            <span>Token</span>
            <span>Amount</span>
          </div>
          {noResults && (
            <p className={styles.noResults}>No results matching your query</p>
          )}
          {!noResults &&
            assets.map((asset) => (
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
