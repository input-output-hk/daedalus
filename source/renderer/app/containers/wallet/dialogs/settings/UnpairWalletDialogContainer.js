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
const react_intl_1 = require('react-intl');
const WalletSettingsRemoveConfirmationDialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletSettingsRemoveConfirmationDialog')
);
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.settings.unpair.dialog.title',
    defaultMessage: '!!!Unpair Wallet',
    description: 'Title for the "Unpair wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.unpair.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description:
      'Label for the "Unpair (x)" button in the unpair wallet dialog.',
  },
  confirmationQuestion: {
    id: 'wallet.settings.unpair.dialog.confirmationQuestion',
    defaultMessage:
      '!!!Do you really want to unpair <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to unpair the wallet.',
  },
});
let UnpairWalletDialogContainer = class UnpairWalletDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  render() {
    const { actions, stores } = this.props;
    const { uiDialogs, wallets, app } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { deleteWalletRequest, isDeleting } = wallets;
    const {
      environment: { isTest },
    } = app;
    // Guard against potential null values
    if (!activeWallet)
      throw new Error(
        'Active wallet required for DeleteWalletDialogContainer.'
      );
    return react_1.default.createElement(
      WalletSettingsRemoveConfirmationDialog_1.default,
      {
        isTest: isTest,
        isUnpair: true,
        walletName: activeWallet.name,
        hasWalletFunds: activeWallet.hasFunds,
        countdownFn: uiDialogs.countdownSinceDialogOpened,
        isBackupNoticeAccepted: dialogData.isBackupNoticeAccepted,
        messages: Object.freeze(messages),
        onAcceptBackupNotice: () =>
          updateDataForActiveDialog.trigger({
            data: {
              isBackupNoticeAccepted: true,
            },
          }),
        onContinue: () => {
          actions.wallets.deleteWallet.trigger({
            walletId: activeWallet.id,
            isLegacy: activeWallet.isLegacy,
          });
        },
        onCancel: () => {
          actions.dialogs.closeActiveDialog.trigger();
          deleteWalletRequest.reset();
        },
        confirmationValue: dialogData.confirmationValue,
        onConfirmationValueChange: (confirmationValue) =>
          updateDataForActiveDialog.trigger({
            data: {
              confirmationValue,
            },
          }),
        isSubmitting: isDeleting,
      }
    );
  }
};
UnpairWalletDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('actions', 'stores'), mobx_react_1.observer],
  UnpairWalletDialogContainer
);
exports.default = UnpairWalletDialogContainer;
//# sourceMappingURL=UnpairWalletDialogContainer.js.map
