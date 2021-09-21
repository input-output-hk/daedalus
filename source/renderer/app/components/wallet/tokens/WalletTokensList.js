// @flow
import React, { useState, useMemo, useCallback } from 'react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { searchAssets } from '../../../utils/assets';
import styles from './WalletTokensList.scss';
import Wallet from '../../../domains/Wallet';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletToken from './WalletToken';
import type { AssetToken } from '../../../api/assets/types';
import sortIcon from '../../../assets/images/ascending.inline.svg';

const messages = defineMessages({
  noResults: {
    id: 'wallet.tokens.list.search.noResults',
    defaultMessage: '!!!No results matching your query',
    description: 'No results on the WalletTokensList',
  },
  searchResults: {
    id: 'wallet.tokens.list.search.searchResults',
    defaultMessage: '!!!Search Results',
    description: 'Search Results on the WalletTokensList',
  },
  columnAmount: {
    id: 'wallet.tokens.list.column.amount',
    defaultMessage: '!!!Amount',
    description: 'Amount header on the WalletTokensList',
  },
  columnToken: {
    id: 'wallet.tokens.list.column.token',
    defaultMessage: '!!!Token',
    description: 'Token header on the WalletTokensList',
  },
  viewAllButtonLabel: {
    id: 'wallet.tokens.list.viewAllButton.label',
    defaultMessage: '!!!View all tokens',
    description: 'View all button label on the WalletTokensList',
  },
});

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  insertingAssetUniqueId: ?string,
  intl: intlShape.isRequired,
  isLoadingAssets: boolean,
  onAssetSettings: Function,
  onCopyAssetParam: Function,
  onOpenAssetSend: Function,
  onToggleFavorite: Function,
  onViewAllButtonClick?: Function,
  removingAssetUniqueId: ?string,
  searchValue?: string,
  title: string,
  tokenFavorites: Object,
  wallet: Wallet,
};

type SortBy = 'token' | 'amount';
type SortDirection = 'asc' | 'desc';

const getSortClasses = (
  item: SortBy,
  sortBy: SortBy,
  sortDirection: SortDirection
) => {
  const isSorted = item === sortBy;
  return classnames([
    styles.sortIcon,
    isSorted ? styles.sorted : null,
    isSorted ? styles[`${sortDirection}Sorting`] : styles.ascSorting,
  ]);
};

const WalletTokensList = observer((props: Props) => {
  const [sortDirection, setSortDirection] = useState<SortDirection>('asc');
  const [sortBy, setSortBy] = useState<SortBy>('token');
  const {
    assets,
    assetSettingsDialogWasOpened,
    insertingAssetUniqueId,
    intl,
    isLoadingAssets,
    onAssetSettings,
    onCopyAssetParam,
    onOpenAssetSend,
    onToggleFavorite,
    onViewAllButtonClick,
    removingAssetUniqueId,
    searchValue = '',
    title,
    tokenFavorites,
    wallet,
  } = props;
  const isRestoreActive = wallet.isRestoring;
  const sortedAssets = useMemo(() => {
    return [...assets].sort((asset1: AssetToken, asset2: AssetToken) => {
      if (sortBy === 'token') {
        if (sortDirection === 'asc') {
          return asset1.fingerprint.localeCompare(asset2.fingerprint);
        }
        return asset2.fingerprint.localeCompare(asset1.fingerprint);
      }
      if (sortDirection === 'asc') {
        return asset1.quantity.isLessThan(asset2.quantity) ? -1 : 1;
      }
      return asset1.quantity.isLessThan(asset2.quantity) ? 1 : -1;
    });
  }, [sortBy, sortDirection]);
  const filteredAssets = searchAssets(searchValue, sortedAssets);
  const hasSearch =
    !isLoadingAssets && !!searchValue && searchValue.trim().length >= 3;
  const noResults = hasSearch && !filteredAssets.length;
  const viewAllButtonStyles = classnames(['flat', styles.viewAllButton]);

  const sortIconClassesToken = useMemo(
    () => getSortClasses('token', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const sortIconClassesAmount = useMemo(
    () => getSortClasses('amount', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const toggleSortDirection = () => {
    if (sortDirection === 'asc') {
      setSortDirection('desc');
    } else {
      setSortDirection('asc');
    }
  };
  const onSortBy = (newSortBy: SortBy) => {
    if (newSortBy === sortBy) {
      toggleSortDirection();
    } else {
      setSortDirection('asc');
      setSortBy(newSortBy);
    }
  };
  const onSortByToken = useCallback(() => onSortBy('token'), [
    sortDirection,
    sortBy,
  ]);
  const onSortByAmount = useCallback(() => onSortBy('amount'), [
    sortDirection,
    sortBy,
  ]);

  let content;
  if (isLoadingAssets) {
    content = (
      <div className={styles.syncingWrapper}>
        <LoadingSpinner big />
      </div>
    );
  } else if (noResults) {
    content = (
      <p className={styles.noResults}>
        {intl.formatMessage(messages.noResults)}
      </p>
    );
  } else {
    content = filteredAssets.map((asset) => {
      return (
        <WalletToken
          key={asset.uniqueId}
          asset={asset}
          onOpenAssetSend={onOpenAssetSend}
          onCopyAssetParam={onCopyAssetParam}
          onAssetSettings={onAssetSettings}
          anyAssetWasHovered
          isLoading={isRestoreActive}
          assetSettingsDialogWasOpened={assetSettingsDialogWasOpened}
          onToggleFavorite={onToggleFavorite}
          isFavorite={tokenFavorites[asset.uniqueId]}
          isInsertingAsset={insertingAssetUniqueId === asset.uniqueId}
          isRemovingAsset={removingAssetUniqueId === asset.uniqueId}
        />
      );
    });
  }

  return (
    <div className={styles.component}>
      <div className={styles.outerHeader}>
        <div className={styles.title}>
          {title}
          {hasSearch && !noResults && (
            <>
              {intl.formatMessage(messages.searchResults)} (
              {filteredAssets.length})
            </>
          )}
        </div>
      </div>
      <BorderedBox>
        <div className={styles.columns}>
          <div onClick={onSortByToken}>
            {intl.formatMessage(messages.columnToken)}
            <SVGInline svg={sortIcon} className={sortIconClassesToken} />
          </div>
          <div onClick={onSortByAmount}>
            {intl.formatMessage(messages.columnAmount)}
            <SVGInline svg={sortIcon} className={sortIconClassesAmount} />
          </div>
        </div>
        {content}
        {onViewAllButtonClick && (
          <Button
            className={viewAllButtonStyles}
            onClick={onViewAllButtonClick}
            label={intl.formatMessage(messages.viewAllButtonLabel)}
          />
        )}
      </BorderedBox>
    </div>
  );
});

export default injectIntl(WalletTokensList);
