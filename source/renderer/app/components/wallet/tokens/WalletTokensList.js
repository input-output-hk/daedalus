// @flow
import React from 'react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { searchAssets } from '../../../utils/assets';
import styles from './WalletTokensList.scss';
import Wallet from '../../../domains/Wallet';
import BorderedBox from '../../widgets/BorderedBox';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import WalletToken from './WalletToken';
import type { AssetToken } from '../../../api/assets/types';

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

const WalletTokensList = observer((props: Props) => {
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
  const filteredAssets = searchAssets(searchValue, assets);
  const hasSearch =
    !isLoadingAssets && !!searchValue && searchValue.trim().length >= 3;
  const noResults = hasSearch && !filteredAssets.length;
  const viewAllButtonStyles = classnames(['flat', styles.viewAllButton]);

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
          <span>{intl.formatMessage(messages.columnToken)}</span>
          <span>{intl.formatMessage(messages.columnAmount)}</span>
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
