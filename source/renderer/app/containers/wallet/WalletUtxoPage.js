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
const WalletUtxo_1 = __importDefault(
  require('../../components/wallet/utxo/WalletUtxo')
);
const utxoUtils_1 = require('../../utils/utxoUtils');
let WalletUtxoPage = class WalletUtxoPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  componentDidMount() {
    this.props.actions.walletSettings.startWalletUtxoPolling.trigger();
  }
  componentWillUnmount() {
    this.props.actions.walletSettings.stopWalletUtxoPolling.trigger();
  }
  render() {
    const { app, wallets, walletSettings, transactions } = this.props.stores;
    const { walletUtxos } = walletSettings;
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletUtxoPage.');
    const distribution = (0, lodash_1.get)(walletUtxos, 'distribution', {});
    const chartData = (0, utxoUtils_1.getUtxoChartData)(distribution);
    const walletUtxosAmount = (0, utxoUtils_1.getWalletUtxosTotalAmount)(
      distribution
    );
    const { pendingTransactionsCount: pendingTxnsCount } = transactions;
    const { getWalletUtxosRequest } = walletSettings;
    const isLoadingInitialUtxoData =
      !getWalletUtxosRequest.wasExecuted ||
      getWalletUtxosRequest.isExecutingFirstTime;
    return react_1.default.createElement(WalletUtxo_1.default, {
      isLoadingInitialUtxoData: isLoadingInitialUtxoData,
      walletAmount: activeWallet.amount,
      walletUtxosAmount: walletUtxosAmount,
      chartData: chartData,
      onExternalLinkClick: app.openExternalLink,
      pendingTxnsCount: pendingTxnsCount,
    });
  }
};
WalletUtxoPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletUtxoPage
);
exports.default = WalletUtxoPage;
//# sourceMappingURL=WalletUtxoPage.js.map
