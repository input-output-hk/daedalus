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
const mobx_react_1 = require('mobx-react');
const AssetSettingsDialog_1 = __importDefault(
  require('../../components/assets/AssetSettingsDialog')
);
const WalletTokens_1 = __importDefault(
  require('../../components/wallet/tokens/wallet-tokens/WalletTokens')
);
const assets_1 = require('../../utils/assets');
const WalletTokensPage = (0, mobx_react_1.inject)(
  'stores',
  'actions'
)(
  (0, mobx_react_1.observer)((props) => {
    const { actions, stores } = props;
    const { assets, profile, wallets, app } = stores;
    const {
      all,
      favorites,
      insertingAssetUniqueId,
      removingAssetUniqueId,
    } = assets;
    const {
      setEditedAsset,
      onOpenAssetSend,
      onToggleFavorite,
    } = actions.assets;
    const { open } = actions.dialogs;
    const { active: activeWallet } = wallets;
    const { currentLocale } = profile;
    const openAssetSettingsDialog = (0, react_1.useCallback)(
      ({ asset }) => {
        setEditedAsset.trigger({
          asset,
        });
        open.trigger({
          dialog: AssetSettingsDialog_1.default,
        });
      },
      [open, setEditedAsset]
    );
    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    const walletTokens = activeWallet.assets.total;
    const assetTokens = (0, assets_1.getAssetTokens)(all, walletTokens);
    const totalRawAssets = activeWallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = activeWallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    return react_1.default.createElement(WalletTokens_1.default, {
      assets: assetTokens,
      currentLocale: currentLocale,
      insertingAssetUniqueId: insertingAssetUniqueId,
      isLoadingAssets: isLoadingAssets,
      onAssetSettings: openAssetSettingsDialog,
      onCopyAssetParam: () => {},
      onOpenAssetSend: onOpenAssetSend.trigger,
      onToggleFavorite: onToggleFavorite.trigger,
      removingAssetUniqueId: removingAssetUniqueId,
      onExternalLinkClick: app.openExternalLink,
      tokenFavorites: favorites,
      wallet: activeWallet,
    });
  })
);
exports.default = WalletTokensPage;
//# sourceMappingURL=WalletTokensPage.js.map
