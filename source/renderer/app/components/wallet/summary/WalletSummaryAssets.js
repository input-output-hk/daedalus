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
    defaultMessage: '!!!Search tokens',
    description: 'Search placeholder on Wallet summary assets page',
  },
  headerToken: {
    id: 'wallet.summary.assets.header.token',
    defaultMessage: '!!!Token',
    description: 'Token header on Wallet summary assets page',
  },
  headerAmount: {
    id: 'wallet.summary.assets.header.amount',
    defaultMessage: '!!!Amount',
    description: 'Amount header on Wallet summary assets page',
  },
  noResults: {
    id: 'wallet.summary.assets.search.noResults',
    defaultMessage: '!!!No results matching your query',
    description: 'No results on Wallet summary assets page',
  },
});

type Props = {
  wallet: Wallet,
  assets: Array<AssetToken>,
  onOpenAssetSend: Function,
  onCopyAssetItem: Function,
  onAssetSettings: Function,
  currentLocale: string,
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

  componentDidUpdate(prevProps: Props) {
    const { wallet } = this.props;
    if (prevProps.wallet.id !== wallet.id) {
      this.setSearchValue('');
      this.focusInputField();
    }
  }

  searchInput: Input;

  handleHoverAsset = () => {
    this.setState({ anyAssetWasHovered: true });
  };

  focusInputField = () => {
    const { isSearchOpen } = this.state;
    if (isSearchOpen && this.searchInput && this.searchInput.focus) {
      this.searchInput.focus();
    }
  };

  toggleSearch = () => {
    const { isSearchOpen } = this.state;
    this.setState(
      {
        isSearchOpen: !isSearchOpen,
      },
      () => {
        this.focusInputField();
      }
    );
  };

  setSearchValue = (searchValue: string) => {
    this.setState({
      searchValue,
    });
  };

  get filteredAssets() {
    const { searchValue } = this.state;
    const { assets } = this.props;
    return onSearchAssetsDropdown(
      searchValue.trim(),
      assets.map((asset) => ({ asset }))
    );
  }

  get assets() {
    const { filteredAssets } = this;
    return filteredAssets.map(({ asset }) => asset);
  }

  renderContent = () => {
    const {
      wallet,
      onOpenAssetSend,
      onCopyAssetItem,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
    } = this.props;
    const { intl } = this.context;
    const { assets } = this;
    const { anyAssetWasHovered, searchValue } = this.state;
    const isRestoreActive = wallet.isRestoring;
    const noResults =
      !assets.length && searchValue && searchValue.trim().length >= 3;

    if (isLoadingAssets) {
      return (
        <div className={styles.syncingWrapper}>
          <LoadingSpinner big />
        </div>
      );
    }

    if (noResults) {
      return (
        <p className={styles.noResults}>
          {intl.formatMessage(messages.noResults)}
        </p>
      );
    }

    return assets.map((asset) => (
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
    ));
  };

  renderHeader = () => {
    const { isLoadingAssets, currentLocale } = this.props;
    const { isSearchOpen, searchValue } = this.state;
    const { intl } = this.context;
    const { assets } = this;
    const searchButtonStyles = classNames([styles.searchButton, 'flat']);
    const numberOfAssets = !isLoadingAssets && !!assets && `(${assets.length})`;

    const searchButtonLabel = isSearchOpen
      ? intl.formatMessage(messages.hideSearchButtonLabel)
      : intl.formatMessage(messages.showSearchButtonLabel);

    const hasSearch =
      !!searchValue && !!searchValue.trim().length && !isSearchOpen;

    const headerStyles = classNames([styles.header, styles[currentLocale]]);

    return (
      <div className={headerStyles}>
        <div className={styles.title}>
          {intl.formatMessage(messages.tokensTitle)} {numberOfAssets}
          {hasSearch && (
            <>
              &nbsp;-&nbsp;
              <div className={styles.searchValue} onClick={this.toggleSearch}>
                {searchValue}
              </div>
              <div
                className={styles.clearSearchValue}
                onClick={() => this.setSearchValue('')}
              >
                <SVGInline svg={crossIcon} />
              </div>
            </>
          )}
        </div>
        <Button
          className={searchButtonStyles}
          onClick={this.toggleSearch}
          label={searchButtonLabel}
          disabled={isLoadingAssets}
        />
      </div>
    );
  };

  renderSearch = () => {
    const { isSearchOpen, searchValue } = this.state;
    const { intl } = this.context;

    if (!isSearchOpen) return null;
    return (
      <div className={styles.search}>
        <SVGInline svg={searchIcon} className={styles.searchIcon} />
        <Input
          className={styles.spendingPassword}
          onChange={this.setSearchValue}
          value={searchValue}
          placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
          ref={(input) => {
            this.searchInput = input;
          }}
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
    );
  };

  render() {
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        {this.renderHeader()}
        {this.renderSearch()}
        <BorderedBox>
          <div className={styles.assetsColumns}>
            <span>{intl.formatMessage(messages.headerToken)}</span>
            <span>{intl.formatMessage(messages.headerAmount)}</span>
          </div>
          {this.renderContent()}
        </BorderedBox>
      </div>
    );
  }
}
