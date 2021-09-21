// @flow
import React, { useState } from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './WalletTokens.scss';
import Wallet from '../../../domains/Wallet';
import WalletTokensList from './WalletTokensList';
import WalletTokensSearch from './WalletTokensSearch';
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
});

type Props = {
  assets: Array<AssetToken>,
  assetSettingsDialogWasOpened: boolean,
  currentLocale: string,
  intl: intlShape.isRequired,
  isLoadingAssets: boolean,
  onAssetSettings: Function,
  onCopyAssetParam: Function,
  onOpenAssetSend: Function,
  onToggleFavorite: Function,
  tokenFavorites: Object,
  wallet: Wallet,
};

type SearchValue = string;
const WalletTokens = observer((props: Props) => {
  const [searchValue, setSearchValue] = useState<SearchValue>('');
  const [insertingAssetUniqueId, setInsertingAssetUniqueId] = useState<?string>(
    null
  );
  const [removingAssetUniqueId, setRemovingAssetUniqueId] = useState<?string>(
    null
  );

  const {
    assets,
    intl,
    tokenFavorites,
    onToggleFavorite,
    ...listProps
  } = props;
  const favoriteTokensList = assets.filter(
    ({ uniqueId }) => tokenFavorites[uniqueId]
  );

  /**
   *
   * This function adds a `inserting` or `removing`
   * state before actually proceding with these actions
   * so the UI element insertion/removal can be animated,
   * preventing undesirable jumps in the tokens list
   *
   */
  const handleToggleFavorite = async ({
    uniqueId,
    isFavorite,
  }: {
    uniqueId: string,
    isFavorite: boolean,
  }) => {
    if (insertingAssetUniqueId || removingAssetUniqueId) {
      return;
    }
    if (isFavorite) {
      // It's removing favorite
      // We need to wait for the element to be removed, before updating the favorites list
      setRemovingAssetUniqueId(uniqueId);
      setTimeout(async () => {
        await onToggleFavorite({ uniqueId, isFavorite });
        setRemovingAssetUniqueId(null);
      }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
    } else {
      // It's inserting favorite
      // We update the favorites list straight away
      setInsertingAssetUniqueId(uniqueId);
      await onToggleFavorite({ uniqueId, isFavorite });
      setTimeout(() => {
        setInsertingAssetUniqueId(null);
      }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
    }
  };

  return (
    <div className={styles.component}>
      <WalletTokensSearch searchValue={searchValue} onSearch={setSearchValue} />
      {!!favoriteTokensList.length && (
        <WalletTokensList
          {...listProps}
          assets={favoriteTokensList}
          title={intl.formatMessage(messages.favoritesListTitle)}
          insertingAssetUniqueId={insertingAssetUniqueId}
          removingAssetUniqueId={removingAssetUniqueId}
          onToggleFavorite={handleToggleFavorite}
          tokenFavorites={tokenFavorites}
        />
      )}
      <WalletTokensList
        {...listProps}
        assets={assets}
        title={intl.formatMessage(messages.tokensListTitle)}
        onToggleFavorite={handleToggleFavorite}
        tokenFavorites={tokenFavorites}
      />
    </div>
  );
});

export default injectIntl(WalletTokens);
