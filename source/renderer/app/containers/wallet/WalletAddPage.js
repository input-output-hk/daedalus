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
const WalletAdd_1 = __importDefault(
  require('../../components/wallet/WalletAdd')
);
const WalletBackupDialog_1 = __importDefault(
  require('../../components/wallet/WalletBackupDialog')
);
const WalletBackupDialogContainer_1 = __importDefault(
  require('./dialogs/WalletBackupDialogContainer')
);
const WalletCreateDialogContainer_1 = __importDefault(
  require('./dialogs/WalletCreateDialogContainer')
);
const WalletRestoreDialogContainer_1 = __importDefault(
  require('./dialogs/WalletRestoreDialogContainer')
);
const WalletConnectDialog_1 = __importDefault(
  require('../../components/wallet/WalletConnectDialog')
);
const WalletConnectDialogContainer_1 = __importDefault(
  require('./dialogs/WalletConnectDialogContainer')
);
const MainLayout_1 = __importDefault(require('../MainLayout'));
// TODO: Remove once the new wallet creation process is ready
const WalletCreateDialogContainerOld_1 = __importDefault(
  require('./dialogs/WalletCreateDialogContainerOld')
);
const WalletCreateDialog_1 = __importDefault(
  require('../../components/wallet/WalletCreateDialog')
);
// TODO: Remove once the new wallet restoration process is ready
const WalletRestoreDialogContainerOld_1 = __importDefault(
  require('./dialogs/WalletRestoreDialogContainerOld')
);
const WalletRestoreDialog_1 = __importDefault(
  require('../../components/wallet/WalletRestoreDialog')
);
const WalletImportDialogContainer_1 = __importDefault(
  require('./dialogs/WalletImportDialogContainer')
);
let WalletAddPage = class WalletAddPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };
  onCloseWalletPairing = () => {
    this.props.stores.hardwareWallets.resetWalletPairing();
    this.onClose();
  };
  render() {
    const { actions, stores } = this.props;
    const { wallets, walletMigration, uiDialogs } = stores;
    const {
      createWalletStep,
      createWalletUseNewProcess,
      restoreWalletStep,
      restoreWalletUseNewProcess,
      environment,
    } = wallets;
    const { walletMigrationStep } = walletMigration;
    const { isMainnet, isTestnet, isProduction } = environment;
    const onCreateWallet = createWalletUseNewProcess
      ? () => actions.wallets.createWalletBegin.trigger() // TODO: Remove once the new wallet creation process is ready
      : () =>
          actions.dialogs.open.trigger({
            dialog: WalletCreateDialog_1.default,
          });
    const onRestoreWallet = restoreWalletUseNewProcess
      ? () => actions.wallets.restoreWalletBegin.trigger() // TODO: Remove once the new wallet restoration process is ready
      : () =>
          actions.dialogs.open.trigger({
            dialog: WalletRestoreDialog_1.default,
          });
    const onImportWallet = () =>
      actions.walletMigration.initiateMigration.trigger();
    const onConnectWallet = () => {
      actions.dialogs.open.trigger({
        dialog: WalletConnectDialog_1.default,
      });
      stores.hardwareWallets.initiateWalletPairing();
    };
    let activeDialog = null;
    // TODO: Remove once the new wallet creation process is ready
    if (uiDialogs.isOpen(WalletCreateDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        WalletCreateDialogContainerOld_1.default,
        { onClose: this.onClose }
      );
    } else if (createWalletStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = react_1.default.createElement(
        WalletCreateDialogContainer_1.default,
        { onClose: this.onClose }
      );
    } else if (uiDialogs.isOpen(WalletBackupDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        WalletBackupDialogContainer_1.default,
        { onClose: this.onClose }
      );
    } else if (uiDialogs.isOpen(WalletRestoreDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        WalletRestoreDialogContainerOld_1.default,
        { onClose: this.onClose }
      );
    } else if (restoreWalletStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = react_1.default.createElement(
        WalletRestoreDialogContainer_1.default,
        { onClose: this.onClose }
      );
    } else if (walletMigrationStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = react_1.default.createElement(
        WalletImportDialogContainer_1.default,
        { onClose: this.onClose }
      );
    } else if (uiDialogs.isOpen(WalletConnectDialog_1.default)) {
      activeDialog = react_1.default.createElement(
        WalletConnectDialogContainer_1.default,
        { onClose: this.onCloseWalletPairing }
      );
    }
    return react_1.default.createElement(
      MainLayout_1.default,
      null,
      react_1.default.createElement(WalletAdd_1.default, {
        onCreate: onCreateWallet,
        onRestore: onRestoreWallet,
        onImport: onImportWallet,
        onConnect: onConnectWallet,
        isMaxNumberOfWalletsReached: wallets.hasMaxWallets,
        isMainnet: isMainnet,
        isTestnet: isTestnet,
        isProduction: isProduction,
      }),
      activeDialog
    );
  }
};
WalletAddPage = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  WalletAddPage
);
exports.default = WalletAddPage;
//# sourceMappingURL=WalletAddPage.js.map
