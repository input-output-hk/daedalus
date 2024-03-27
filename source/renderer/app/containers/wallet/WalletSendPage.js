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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const numbersConfig_1 = require('../../config/numbersConfig');
const WalletSendForm_1 = __importDefault(
  require('../../components/wallet/WalletSendForm')
);
const SendConfirmation_view_1 = require('./dialogs/send-confirmation/SendConfirmation.view');
const WalletTokenPicker_1 = __importDefault(
  require('../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker')
);
const walletsConfig_1 = require('../../config/walletsConfig');
const assets_1 = require('../../utils/assets');
const withAnalytics_1 = require('../../components/analytics/withAnalytics');
let WalletSendPage = class WalletSendPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  state = {
    confirmationDialogData: null,
  };
  calculateTransactionFee = async (params) => {
    const {
      walletId,
      address,
      amount,
      isHardwareWallet,
      selectedAssets,
    } = params;
    if (isHardwareWallet) {
      const coinSelection = await this.props.stores.hardwareWallets.selectCoins(
        {
          walletId,
          address,
          amount,
          assets: selectedAssets,
        }
      );
      return {
        fee: coinSelection.fee,
        coinSelection,
      };
    }
    const {
      fee,
      minimumAda,
    } = await this.props.stores.transactions.calculateTransactionFee({
      walletId,
      address,
      amount,
      assets: selectedAssets,
    });
    return {
      fee,
      minimumAda,
    };
  };
  submit = (isHardwareWallet, walletId, { coinSelection, ...data }) => {
    const { isFlight } = global;
    if (isHardwareWallet) {
      this.props.stores.hardwareWallets.updateTxSignRequest(coinSelection);
    }
    this.props.actions.dialogs.open.trigger({
      dialog: SendConfirmation_view_1.WalletSendConfirmationDialogView,
    });
    if (isHardwareWallet && !isFlight) {
      this.props.stores.hardwareWallets.initiateTransaction({
        walletId,
      });
    }
    this.setState({ confirmationDialogData: { ...data } });
  };
  openTokenPickerDialog = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletTokenPicker_1.default,
    });
  };
  closeTokenPickerDialog = () => {
    const { actions, stores } = this.props;
    if (!stores.uiDialogs.isOpen(WalletTokenPicker_1.default)) return;
    actions.dialogs.closeActiveDialog.trigger();
  };
  getAssetByUniqueId = (uniqueId, allAssets) => {
    return allAssets.find((asset) => asset.uniqueId === uniqueId);
  };
  render() {
    const { stores, actions } = this.props;
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
      hardwareWallets,
      assets: assetsStore,
    } = stores;
    const { isValidAddress, isAddressFromSameWallet } = wallets;
    const { validateAmount, validateAssetAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const hasAssetsEnabled = walletsConfig_1.WALLET_ASSETS_ENABLED;
    const { all: allAssets, activeAsset, getAsset, favorites } = assetsStore;
    const { unsetActiveAsset } = actions.wallets;
    const selectedAsset = activeAsset
      ? this.getAssetByUniqueId(activeAsset, allAssets)
      : null;
    // Guard against potential null values
    const wallet = wallets.active;
    if (!wallet) throw new Error('Active wallet required for WalletSendPage.');
    const { isHardwareWallet, name: walletName } = wallet;
    const walletTokens = wallet.assets.total;
    const assetTokens = (0, assets_1.getNonZeroAssetTokens)(
      walletTokens,
      getAsset
    );
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    return react_1.default.createElement(WalletSendForm_1.default, {
      currencyMaxIntegerDigits: numbersConfig_1.MAX_INTEGER_PLACES_IN_ADA,
      currencyMaxFractionalDigits: numbersConfig_1.DECIMAL_PLACES_IN_ADA,
      currentNumberFormat: profile.currentNumberFormat,
      calculateTransactionFee: (address, amount, selectedAssets) =>
        this.calculateTransactionFee({
          walletId: wallet.id,
          address,
          amount,
          isHardwareWallet,
          selectedAssets,
        }),
      walletAmount: wallet.amount,
      validateAmount: validateAmount,
      validateAssetAmount: validateAssetAmount,
      addressValidator: isValidAddress,
      assets: assetTokens,
      hasAssets: hasAssetsEnabled && hasRawAssets,
      selectedAsset: selectedAsset,
      isLoadingAssets: isLoadingAssets,
      isDialogOpen: uiDialogs.isOpen,
      isRestoreActive: wallet.isRestoring,
      isHardwareWallet: isHardwareWallet,
      hwDeviceStatus: hwDeviceStatus,
      onSubmit: (data) => this.submit(isHardwareWallet, wallet.id, data),
      onUnsetActiveAsset: unsetActiveAsset.trigger,
      onExternalLinkClick: app.openExternalLink,
      isAddressFromSameWallet: isAddressFromSameWallet,
      tokenFavorites: favorites,
      walletName: walletName,
      onTokenPickerDialogOpen: this.openTokenPickerDialog,
      onTokenPickerDialogClose: this.closeTokenPickerDialog,
      analyticsTracker: this.props.analyticsTracker,
      confirmationDialogData: this.state.confirmationDialogData,
    });
  }
};
WalletSendPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletSendPage
);
exports.default = (0, withAnalytics_1.withAnalytics)(WalletSendPage);
//# sourceMappingURL=WalletSendPage.js.map
