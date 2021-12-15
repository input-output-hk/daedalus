// @flow
import React, { useState, useMemo, useCallback } from 'react';
import { intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { searchAssets, sortAssets } from '../../../../utils/assets';
import styles from './WalletTokensList.scss';
import Wallet from '../../../../domains/Wallet';
import BorderedBox from '../../../widgets/BorderedBox';
import LoadingSpinner from '../../../widgets/LoadingSpinner';
import WalletToken from '../wallet-token/WalletToken';
import WalletNoTokens from '../wallet-no-tokens/WalletNoTokens';
import sortIcon from '../../../../assets/images/ascending.inline.svg';
import { messages } from './WalletTokensList.messages';
import type { AssetToken } from '../../../../api/assets/types';
import type { SortBy, SortDirection } from '../../../../utils/assets';

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  insertingAssetUniqueId: ?string,
  intl: intlShape.isRequired,
  isLoadingAssets: boolean,
  onAssetSettings: Function,
  onCopyAssetParam: Function,
  onExternalLinkClick: Function,
  onOpenAssetSend: Function,
  onToggleFavorite: Function,
  onViewAllButtonClick?: Function,
  removingAssetUniqueId: ?string,
  searchValue?: string,
  title: string,
  tokenFavorites: Object,
  wallet: Wallet,
};

const getSortIconClasses = (
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
    onExternalLinkClick,
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
    return [...assets].sort(sortAssets(sortBy, sortDirection));
  }, [assets, sortBy, sortDirection]);
  const filteredAssets = searchAssets(searchValue, sortedAssets) || [];
  const hasSearch =
    !isLoadingAssets && !!searchValue && searchValue.trim().length >= 3;
  const noResults = hasSearch && !filteredAssets.length;
  const viewAllButtonStyles = classnames(['flat', styles.viewAllButton]);
  const hasSorting = filteredAssets.length && filteredAssets.length > 1;
  const columnsStyles = classnames([
    styles.columns,
    hasSorting ? styles.hasSorting : null,
  ]);
  const sortIconClassesToken = useMemo(
    () => getSortIconClasses('token', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const sortIconClassesAmount = useMemo(
    () => getSortIconClasses('quantity', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const toggleSortDirection = useCallback(() => {
    if (sortDirection === 'asc') {
      setSortDirection('desc');
    } else {
      setSortDirection('asc');
    }
  }, [sortDirection]);
  const onSortBy = useCallback(
    (newSortBy: SortBy) => {
      if (!hasSorting) return;
      if (newSortBy === sortBy) {
        toggleSortDirection();
      } else {
        setSortDirection('asc');
        setSortBy(newSortBy);
      }
    },
    [sortDirection, hasSorting, sortBy]
  );
  const onSortByToken = useCallback(() => onSortBy('token'), [
    sortDirection,
    sortBy,
    hasSorting,
  ]);
  const onSortByAmount = useCallback(() => onSortBy('quantity'), [
    sortDirection,
    sortBy,
    hasSorting,
  ]);
  const hasTokens = assets.length || isLoadingAssets;

  if (!hasTokens)
    return (
      <WalletNoTokens
        numberOfAssets={assets.length}
        isLoadingAssets={isLoadingAssets}
        onExternalLinkClick={onExternalLinkClick}
      />
    );

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
        <div className={columnsStyles}>
          <div className={styles.column} onClick={onSortByToken}>
            {intl.formatMessage(messages.columnToken)}
            <SVGInline svg={sortIcon} className={sortIconClassesToken} />
          </div>
          <div className={styles.column} onClick={onSortByAmount}>
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
