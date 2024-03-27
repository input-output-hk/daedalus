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
const lodash_1 = require('lodash');
const WalletSelectImportDialog_1 = __importDefault(
  require('../../../../components/wallet/wallet-import/WalletSelectImportDialog')
);
const walletExportTypes_1 = require('../../../../types/walletExportTypes');
const numbersConfig_1 = require('../../../../config/numbersConfig');
const validations_1 = require('../../../../utils/validations');
const injectedPropsType_1 = require('../../../../types/injectedPropsType');
const DefaultProps =
  injectedPropsType_1.InjectedDialogContainerStepDefaultProps;
let WalletSelectImportStepContainer = class WalletSelectImportStepContainer extends react_1.Component {
  static defaultProps = DefaultProps;
  state = {
    existingWalletsCount: this.props.stores.wallets.all.length,
  };
  onWalletNameChange = (params) => {
    this.props.actions.walletMigration.updateWalletName.trigger(params);
  };
  onToggleWalletImportSelection = (params) => {
    this.props.actions.walletMigration.toggleWalletImportSelection.trigger(
      params
    );
  };
  render() {
    const { onClose, onContinue, stores } = this.props;
    const { walletMigration, app } = stores;
    const {
      exportedWallets,
      pendingImportWalletsCount,
      isRestorationRunning,
    } = walletMigration;
    const { openExternalLink } = app;
    let walletsCount =
      this.state.existingWalletsCount + pendingImportWalletsCount;
    (0, lodash_1.map)(exportedWallets, (wallet) => {
      if (
        wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.COMPLETED ||
        wallet.import.status ===
          walletExportTypes_1.WalletImportStatuses.RUNNING
      ) {
        walletsCount++;
      }
    });
    const isMaxNumberOfWalletsReached =
      walletsCount >= numbersConfig_1.MAX_ADA_WALLETS_COUNT;
    return react_1.default.createElement(WalletSelectImportDialog_1.default, {
      onClose: onClose,
      onContinue: onContinue,
      isSubmitting: isRestorationRunning,
      nameValidator: (name) => (0, validations_1.isValidWalletName)(name),
      exportedWallets: exportedWallets,
      pendingImportWalletsCount: pendingImportWalletsCount,
      onOpenExternalLink: openExternalLink,
      onWalletNameChange: this.onWalletNameChange,
      onToggleWalletImportSelection: this.onToggleWalletImportSelection,
      isMaxNumberOfWalletsReached: isMaxNumberOfWalletsReached,
    });
  }
};
WalletSelectImportStepContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletSelectImportStepContainer
);
exports.default = WalletSelectImportStepContainer;
//# sourceMappingURL=WalletSelectImportStepContainer.js.map
