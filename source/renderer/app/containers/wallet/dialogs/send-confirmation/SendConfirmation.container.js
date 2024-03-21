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
exports.WalletSendConfirmationDialogContainer = exports.Containter = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const compose_1 = __importDefault(require('lodash/fp/compose'));
const assets_1 = require('../../../../utils/assets');
const SendConfirmation_view_1 = require('./SendConfirmation.view');
function Containter({
  actions,
  stores,
  amount,
  selectedAssets,
  assetsAmounts,
  receiver,
  totalAmount,
  onExternalLinkClick,
  transactionFee,
  hwDeviceStatus,
  isHardwareWallet,
  formattedTotalAmount,
}) {
  const { isFlight } = global;
  const {
    assets: { getAsset },
    wallets: { sendMoneyRequest, active: activeWallet },
    hardwareWallets: {
      _resetTransaction: resetHardwareWalletTransaction,
      sendMoneyRequest: sendMoneyExternalRequest,
      isTransactionPending,
      checkIsTrezorByWalletId,
      initiateTransaction,
    },
  } = stores;
  const {
    assets: { onCopyAssetParam },
    dialogs: { closeActiveDialog },
    hardwareWallets: { sendMoney: hardwareWalletsSendMoney },
    wallets: { sendMoney: walletsSendMoney },
  } = actions;
  if (!activeWallet) {
    throw new Error('Active wallet required for WalletSendPage.');
  }
  const [areTermsAccepted, setAreTermsAccepted] = (0, react_1.useState)(false);
  const assetTokens = (0, react_1.useMemo)(
    () =>
      (0, assets_1.getNonZeroAssetTokens)(activeWallet.assets.total, getAsset),
    [activeWallet.assets.total, getAsset]
  );
  const isSubmitting =
    (!isHardwareWallet && sendMoneyRequest.isExecuting) ||
    (isHardwareWallet &&
      (sendMoneyExternalRequest.isExecuting || isTransactionPending));
  const error = isHardwareWallet
    ? sendMoneyExternalRequest.error
    : sendMoneyRequest.error;
  const onSubmitCb = (0, react_1.useCallback)(
    (values) => {
      if (values.isHardwareWallet) {
        hardwareWalletsSendMoney.trigger();
      } else {
        walletsSendMoney.trigger(values);
      }
    },
    [hardwareWalletsSendMoney, walletsSendMoney]
  );
  const onTermsCheckboxClick = (0, react_1.useCallback)(
    (areTermsAccepted) => {
      setAreTermsAccepted(areTermsAccepted);
      if (isHardwareWallet) {
        initiateTransaction({
          walletId: activeWallet.id,
        });
      }
    },
    [isHardwareWallet, initiateTransaction, setAreTermsAccepted]
  );
  const onCancel = (0, react_1.useCallback)(() => {
    closeActiveDialog.trigger();
    sendMoneyRequest.reset();
    resetHardwareWalletTransaction({
      cancelDeviceAction: true,
    });
  }, [sendMoneyRequest, closeActiveDialog, resetHardwareWalletTransaction]);
  return react_1.default.createElement(
    SendConfirmation_view_1.WalletSendConfirmationDialogView,
    {
      amount: amount,
      assetTokens: assetTokens,
      assetsAmounts: assetsAmounts,
      selectedAssets: selectedAssets,
      areTermsAccepted: areTermsAccepted,
      receiver: receiver,
      hwDeviceStatus: hwDeviceStatus,
      error: error,
      onCopyAssetParam: onCopyAssetParam.trigger,
      formattedTotalAmount: formattedTotalAmount,
      totalAmount: totalAmount,
      transactionFee: transactionFee,
      wallet: activeWallet,
      isFlight: isFlight,
      isSubmitting: isSubmitting,
      isHardwareWallet: isHardwareWallet,
      isTrezor: checkIsTrezorByWalletId(activeWallet.id),
      onCancel: onCancel,
      onSubmitCb: onSubmitCb,
      onTermsCheckboxClick: onTermsCheckboxClick,
      onExternalLinkClick: onExternalLinkClick,
    }
  );
}
exports.Containter = Containter;
exports.WalletSendConfirmationDialogContainer = (0, compose_1.default)(
  (0, mobx_react_1.inject)('actions', 'stores'),
  mobx_react_1.observer
)(Containter);
//# sourceMappingURL=SendConfirmation.container.js.map
