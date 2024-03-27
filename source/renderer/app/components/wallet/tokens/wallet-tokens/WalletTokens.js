'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const WalletTokens_scss_1 = __importDefault(require('./WalletTokens.scss'));
const WalletTokensList_1 = __importDefault(
  require('../wallet-tokens-list/WalletTokensList')
);
const WalletTokensSearch_1 = __importDefault(
  require('../wallet-tokens-search/WalletTokensSearch')
);
const LoadingSpinner_1 = __importDefault(
  require('../../../widgets/LoadingSpinner')
);
const timingConfig_1 = require('../../../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
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
const WalletTokens = (0, mobx_react_1.observer)((props) => {
  const [searchValue, setSearchValue] = (0, react_1.useState)('');
  const [insertingAssetUniqueId, setInsertingAssetUniqueId] = (0,
  react_1.useState)(null);
  const [removingAssetUniqueId, setRemovingAssetUniqueId] = (0,
  react_1.useState)(null);
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
  const favoriteTokensList = (0, react_1.useMemo)(
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
  const handleToggleFavorite = (0, react_1.useCallback)(
    async ({ uniqueId, isFavorite }) => {
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
        }, timingConfig_1.TOGGLE_TOKEN_FAVORITE_TIMEOUT);
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
        }, timingConfig_1.TOGGLE_TOKEN_FAVORITE_TIMEOUT);
      }
    },
    [insertingAssetUniqueId, removingAssetUniqueId]
  );
  if (isRestoring) {
    return react_1.default.createElement(
      'div',
      { className: WalletTokens_scss_1.default.syncing },
      react_1.default.createElement(LoadingSpinner_1.default, { big: true }),
      react_1.default.createElement(
        'p',
        { className: WalletTokens_scss_1.default.syncingText },
        intl.formatMessage(messages.syncingMessage)
      )
    );
  }
  return react_1.default.createElement(
    'div',
    { className: WalletTokens_scss_1.default.component },
    hasTokens &&
      react_1.default.createElement(
        'div',
        { className: WalletTokens_scss_1.default.searchContainer },
        react_1.default.createElement(WalletTokensSearch_1.default, {
          searchValue: searchValue,
          onSearch: setSearchValue,
        })
      ),
    !!favoriteTokensList.length &&
      react_1.default.createElement(WalletTokensList_1.default, {
        ...listProps,
        assets: favoriteTokensList,
        insertingAssetUniqueId: insertingAssetUniqueId,
        onToggleFavorite: handleToggleFavorite,
        removingAssetUniqueId: removingAssetUniqueId,
        searchValue: searchValue,
        title: intl.formatMessage(messages.favoritesListTitle),
        tokenFavorites: tokenFavorites,
      }),
    react_1.default.createElement(WalletTokensList_1.default, {
      ...listProps,
      assets: assets,
      onToggleFavorite: handleToggleFavorite,
      searchValue: searchValue,
      title: intl.formatMessage(messages.tokensListTitle),
      tokenFavorites: tokenFavorites,
    })
  );
});
exports.default = (0, react_intl_1.injectIntl)(WalletTokens);
//# sourceMappingURL=WalletTokens.js.map
