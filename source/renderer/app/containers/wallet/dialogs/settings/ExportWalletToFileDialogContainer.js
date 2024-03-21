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
const path_1 = __importDefault(require('path'));
const mobx_react_1 = require('mobx-react');
const show_file_dialog_channels_1 = require('../../../../ipc/show-file-dialog-channels');
const ExportWalletToFileDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/ExportWalletToFileDialog')
);
let ExportWalletToFileDialogContainer = class ExportWalletToFileDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onSubmit = async (params) => {
    const name = 'wallet-export';
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path_1.default.join(
      desktopDirectoryPath,
      `${name}.json`
    );
    const fileParams = {
      defaultPath,
      filters: [
        {
          name,
          extensions: ['json'],
        },
      ],
    };
    const {
      filePath,
    } = await show_file_dialog_channels_1.showSaveDialogChannel.send(
      fileParams
    );
    const { stores, actions } = this.props;
    const activeWallet = stores.wallets.active;
    if (!filePath || !activeWallet) return;
    actions.walletSettings.exportToFile.trigger({
      walletId: activeWallet.id,
      filePath,
      ...params,
    });
  };
  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
    this.props.stores.walletSettings.exportWalletToFileRequest.reset();
  };
  render() {
    const { wallets, walletSettings } = this.props.stores;
    const activeWallet = wallets.active;
    const { exportWalletToFileRequest } = walletSettings;
    // We need an active wallet
    if (!activeWallet) return null;
    return react_1.default.createElement(ExportWalletToFileDialog_1.default, {
      walletName: activeWallet.name,
      isSubmitting: exportWalletToFileRequest.isExecuting,
      onSubmit: this.onSubmit,
      onClose: this.onCancel,
      error: exportWalletToFileRequest.error,
    });
  }
};
ExportWalletToFileDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  ExportWalletToFileDialogContainer
);
exports.default = ExportWalletToFileDialogContainer;
//# sourceMappingURL=ExportWalletToFileDialogContainer.js.map
