import React, { useState, useCallback, useMemo } from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTokens.scss' or its co... Remove this comment to see the full error message
import styles from './WalletTokens.scss';
import Wallet from '../../../domains/Wallet';
import WalletTokensList from './WalletTokensList';
import WalletTokensSearch from './WalletTokensSearch';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { AssetToken } from '../../../api/assets/types';
import { TOGGLE_TOKEN_FAVORITE_TIMEOUT } from '../../../config/timingConfig';

const messages = defineMessages({
  favoritesListTitle: {
    id: 'wallet.tokens.list.favorites.title',
    defaultMessage: '!!!Favorites',
    description: 'Favorites list title label',
  },
  tokensListTitle: {
    id: 'wallet.tokens.list.tokens.title',
    defaultMessage: '!!!Tokens',
    description: 'Favorites list title label',
  },
  syncingMessage: {
    id: 'wallet.send.form.syncingTransactionsMessage',
    defaultMessage:
      '!!!The balance and transaction history of this wallet is being synced with the blockchain.',
    description:
      'Syncing transactions message shown during async wallet restore in the wallet send form.',
  },
});
type Props = {
  assets: Array<AssetToken>;
  assetSettingsDialogWasOpened: boolean;
  currentLocale: string;
  intl: intlShape.isRequired;
  isLoadingAssets: boolean;
  onAssetSettings: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
  onOpenAssetSend: (...args: Array<any>) => any;
  onToggleFavorite: (...args: Array<any>) => any;
  tokenFavorites: Record<string, any>;
  wallet: Wallet;
};
const WalletTokens = observer((props: Props) => {
  const [searchValue, setSearchValue] = useState<string>('');
  const [insertingAssetUniqueId, setInsertingAssetUniqueId] = useState<
    string | null | undefined
  >(null);
  const [removingAssetUniqueId, setRemovingAssetUniqueId] = useState<
    string | null | undefined
  >(null);
  const {
    assets,
    intl,
    tokenFavorites,
    onToggleFavorite,
    isLoadingAssets,
    ...listProps
  } = props;
  const { isRestoring } = props.wallet;
  const hasTokens = assets.length || isLoadingAssets;
  const favoriteTokensList = useMemo(
    () => assets.filter(({ uniqueId }) => tokenFavorites[uniqueId]),
    [assets, tokenFavorites, searchValue]
  );

  /**
   *
   * This function adds a `inserting` or `removing`
   * state before actually proceding with these actions
   * so the UI element insertion/removal can be animated,
   * preventing undesirable jumps in the tokens list
   *
   */
  const handleToggleFavorite = useCallback(
    async ({
      uniqueId,
      isFavorite,
    }: {
      uniqueId: string;
      isFavorite: boolean;
    }) => {
      if (insertingAssetUniqueId || removingAssetUniqueId) {
        return;
      }

      if (isFavorite) {
        // It's removing favorite
        // We need to wait for the element to be removed, before updating the favorites list
        setRemovingAssetUniqueId(uniqueId);
        setTimeout(async () => {
          await onToggleFavorite({
            uniqueId,
            isFavorite,
          });
          setTimeout(() => setRemovingAssetUniqueId(null), 500);
        }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
      } else {
        // It's inserting favorite
        // We update the favorites list straight away
        setInsertingAssetUniqueId(uniqueId);
        await onToggleFavorite({
          uniqueId,
          isFavorite,
        });
        setTimeout(() => {
          setInsertingAssetUniqueId(null);
        }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
      }
    },
    [insertingAssetUniqueId, removingAssetUniqueId]
  );

  if (isRestoring) {
    return (
      <div className={styles.syncing}>
        <LoadingSpinner big />
        <p className={styles.syncingText}>
          {intl.formatMessage(messages.syncingMessage)}
        </p>
      </div>
    );
  }

  return (
    <div className={styles.component}>
      {hasTokens && (
        <WalletTokensSearch
          searchValue={searchValue}
          onSearch={setSearchValue}
        />
      )}
      {!!favoriteTokensList.length && (
        <WalletTokensList
          {...listProps}
          assets={favoriteTokensList}
          insertingAssetUniqueId={insertingAssetUniqueId}
          onToggleFavorite={handleToggleFavorite}
          removingAssetUniqueId={removingAssetUniqueId}
          searchValue={searchValue}
          title={intl.formatMessage(messages.favoritesListTitle)}
          tokenFavorites={tokenFavorites}
        />
      )}
      <WalletTokensList
        {...listProps}
        assets={assets}
        onToggleFavorite={handleToggleFavorite}
        searchValue={searchValue}
        title={intl.formatMessage(messages.tokensListTitle)}
        tokenFavorites={tokenFavorites}
      />
    </div>
  );
});
export default injectIntl(WalletTokens);
