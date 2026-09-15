import React, { useState, useCallback, useMemo } from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { observer } from 'mobx-react';
import styles from './WalletTokens.scss';
import Wallet from '../../../../domains/Wallet';
import WalletTokensList from '../wallet-tokens-list/WalletTokensList';
import WalletTokensSearch from '../wallet-tokens-search/WalletTokensSearch';
import LoadingSpinner from '../../../widgets/LoadingSpinner';
import type { AssetToken } from '../../../../api/assets/types';
import { TOGGLE_TOKEN_FAVORITE_TIMEOUT } from '../../../../config/timingConfig';

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
  decimalPlacesNotice: {
    id: 'wallet.tokens.decimalPlacesNotice',
    defaultMessage:
      '!!!For tokens whose decimal places an issuer has published and proved, amounts are now entered in those units rather than in the whole units the ledger holds: one and a half of a six-decimal token is now 1.5 and not 1500000. Balances for those tokens are shown the same way. A decimal place setting you have chosen yourself still overrides both.',
    description:
      'One-time notice on the token list, shown after the update that began applying verified decimal places, explaining that the amount field for those tokens now takes issuer units.',
  },
  decimalPlacesNoticeDismiss: {
    id: 'wallet.tokens.decimalPlacesNotice.dismiss',
    defaultMessage: '!!!Got it',
    description:
      'Label for the button that dismisses the one-time notice on the token list.',
  },
});
type Props = {
  assets: Array<AssetToken>;
  currentLocale: string;
  intl: intlShape.isRequired;
  /**
   * Whether this profile has already been told that verified decimal places are
   * applied on their own. Held per profile in browser storage, so dismissing it
   * survives a restart.
   */
  isDecimalPlacesNoticeAcknowledged?: boolean;
  onAcknowledgeDecimalPlacesNotice?: () => void;
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
    isDecimalPlacesNoticeAcknowledged = true,
    onAcknowledgeDecimalPlacesNotice,
    ...listProps
  } = props;
  const { isRestoring } = props.wallet;
  const hasTokens = assets.length > 0;
  // Held tokens, from the wallet rather than from the metadata cache: a profile
  // with nothing to send has no habit to correct, whatever the cache knows.
  const showsDecimalPlacesNotice =
    !isDecimalPlacesNoticeAcknowledged && assets.length > 0;
  const favoriteTokensList = useMemo(
    () => assets.filter(({ uniqueId }) => tokenFavorites[uniqueId]),
    [assets, tokenFavorites, searchValue]
  );

  /**
   *
   * This function adds a `inserting` or `removing`
   * state before actually proceeding with these actions
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
      {showsDecimalPlacesNotice && (
        <div
          className={styles.decimalPlacesNotice}
          data-testid="decimalPlacesNotice"
        >
          <p className={styles.decimalPlacesNoticeText}>
            {intl.formatMessage(messages.decimalPlacesNotice)}
          </p>
          <button
            className={styles.decimalPlacesNoticeDismiss}
            type="button"
            data-testid="decimalPlacesNotice:dismiss"
            onClick={onAcknowledgeDecimalPlacesNotice}
          >
            {intl.formatMessage(messages.decimalPlacesNoticeDismiss)}
          </button>
        </div>
      )}
      {hasTokens && (
        <div className={styles.searchContainer}>
          <WalletTokensSearch
            searchValue={searchValue}
            onSearch={setSearchValue}
          />
        </div>
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
