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
const TransferFundsStep2Dialog_1 = __importDefault(
  require('../../../../components/wallet/transfer-funds/TransferFundsStep2Dialog')
);
const injectedPropsType_1 = require('../../../../types/injectedPropsType');
const DefaultProps =
  injectedPropsType_1.InjectedDialogContainerStepDefaultProps;
let TransferFundsStep2Container = class TransferFundsStep2Container extends react_1.Component {
  static defaultProps = DefaultProps;
  onClose = () => {
    const { onClose } = this.props;
    const { transferFundsRequest } = this.props.stores.wallets;
    if (transferFundsRequest.isExecuting) {
      return;
    }
    transferFundsRequest.reset();
    onClose();
  };
  render() {
    const { stores, actions, onBack } = this.props;
    const {
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
      allLegacyWallets,
      allWallets,
      transferFundsFee,
      transferFundsLeftovers,
      transferFundsRequest,
    } = stores.wallets;
    const { openExternalLink } = stores.app;
    const onFinish = (spendingPassword) =>
      actions.wallets.transferFunds.trigger({
        spendingPassword,
      });
    const sourceWallet = allLegacyWallets.find(
      ({ id }) => id === transferFundsSourceWalletId
    );
    const targetWallet = allWallets.find(
      ({ id }) => id === transferFundsTargetWalletId
    );
    if (
      !sourceWallet ||
      !targetWallet ||
      !transferFundsFee ||
      !transferFundsLeftovers
    )
      return null;
    const sourceWalletName = sourceWallet.name;
    const sourceWalletAmount = sourceWallet.amount;
    const targetWalletName = targetWallet.name;
    return react_1.default.createElement(TransferFundsStep2Dialog_1.default, {
      feesAmount: transferFundsFee,
      leftoversAmount: transferFundsLeftovers,
      sourceWalletAmount: sourceWalletAmount,
      sourceWalletName: sourceWalletName,
      targetWalletName: targetWalletName,
      onOpenExternalLink: openExternalLink,
      onBack: onBack,
      onClose: this.onClose,
      onFinish: onFinish,
      isSubmitting: false,
      error: transferFundsRequest.error,
    });
  }
};
TransferFundsStep2Container = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  TransferFundsStep2Container
);
exports.default = TransferFundsStep2Container;
//# sourceMappingURL=TransferFundsStep2Container.js.map
