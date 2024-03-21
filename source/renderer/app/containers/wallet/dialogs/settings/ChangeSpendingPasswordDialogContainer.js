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
const ChangeSpendingPasswordDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/ChangeSpendingPasswordDialog')
);
let ChangeSpendingPasswordDialogContainer = class ChangeSpendingPasswordDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  render() {
    const { actions } = this.props;
    const { uiDialogs, wallets, walletSettings, profile } = this.props.stores;
    const { currentLocale } = profile;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { updateSpendingPasswordRequest } = walletSettings;
    if (!activeWallet)
      throw new Error(
        'Active wallet required for ChangeSpendingPasswordDialogContainer.'
      );
    return react_1.default.createElement(
      ChangeSpendingPasswordDialog_1.default,
      {
        isSpendingPasswordSet: activeWallet.hasPassword,
        currentPasswordValue: dialogData.currentPasswordValue,
        newPasswordValue: dialogData.newPasswordValue,
        repeatedPasswordValue: dialogData.repeatedPasswordValue,
        onSave: (values) => {
          const { id: walletId, isLegacy } = activeWallet;
          const { oldPassword, newPassword } = values;
          actions.walletSettings.updateSpendingPassword.trigger({
            walletId,
            oldPassword,
            newPassword,
            isLegacy,
          });
        },
        onCancel: () => {
          actions.dialogs.closeActiveDialog.trigger();
          updateSpendingPasswordRequest.reset();
        },
        onDataChange: (data) => {
          updateDataForActiveDialog.trigger({
            data,
          });
        },
        isSubmitting: updateSpendingPasswordRequest.isExecuting,
        error: updateSpendingPasswordRequest.error,
        walletName: activeWallet.name,
        currentLocale: currentLocale,
      }
    );
  }
};
ChangeSpendingPasswordDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  ChangeSpendingPasswordDialogContainer
);
exports.default = ChangeSpendingPasswordDialogContainer;
//# sourceMappingURL=ChangeSpendingPasswordDialogContainer.js.map
