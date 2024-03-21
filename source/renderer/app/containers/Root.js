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
const WalletAddPage_1 = __importDefault(require('./wallet/WalletAddPage'));
const LoadingPage_1 = __importDefault(require('./loading/LoadingPage'));
const SplashNetworkPage_1 = __importDefault(
  require('./splash/SplashNetworkPage')
);
const RedeemItnRewardsContainer_1 = __importDefault(
  require('./staking/RedeemItnRewardsContainer')
);
const AppUpdateContainer_1 = __importDefault(
  require('./appUpdate/AppUpdateContainer')
);
const WalletImportFileDialog_1 = __importDefault(
  require('../components/wallet/wallet-import/WalletImportFileDialog')
);
let Root = class Root extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  render() {
    const { stores, actions, children } = this.props;
    const {
      app,
      appUpdate,
      networkStatus,
      profile,
      staking,
      voting,
      uiDialogs,
      wallets,
    } = stores;
    const { isVotingPage } = voting;
    const { isStakingPage, redeemStep } = staking;
    const { isProfilePage, isSettingsPage } = profile;
    const { displayAppUpdateOverlay } = appUpdate;
    const { hasLoadedWallets } = wallets;
    const {
      isConnected,
      isNodeStopping,
      isNodeStopped,
      isNotEnoughDiskSpace,
      isSplashShown,
      isSystemTimeCorrect,
    } = networkStatus;
    const { isCurrentLocaleSet, areTermsOfUseAccepted } = profile;
    const isWalletImportDialogOpen = uiDialogs.isOpen(
      WalletImportFileDialog_1.default
    );
    const isPageThatDoesntNeedWallets =
      (isStakingPage || isSettingsPage || isVotingPage) &&
      hasLoadedWallets &&
      isConnected;
    // In case node is in stopping sequence we must show the "Connecting" screen
    // with the "Stopping Cardano node..." and "Cardano node stopped" messages
    // for all the screens except of the "Network status" screen.
    const isNodeInStoppingSequence = isNodeStopping || isNodeStopped;
    if (
      isCurrentLocaleSet &&
      areTermsOfUseAccepted &&
      !app.environment.isTest &&
      isSplashShown
    ) {
      return react_1.default.createElement(SplashNetworkPage_1.default, null);
    }
    if (!isNodeInStoppingSequence && redeemStep !== null) {
      return react_1.default.createElement(
        RedeemItnRewardsContainer_1.default,
        null
      );
    }
    if (!isNodeInStoppingSequence && displayAppUpdateOverlay) {
      return react_1.default.createElement(AppUpdateContainer_1.default, null);
    }
    // Just render any page that doesn't require wallets to be loaded or node to be connected
    if (
      (isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
      (isProfilePage && (isNotEnoughDiskSpace || !isNodeInStoppingSequence))
    ) {
      return children;
    }
    if (
      !isConnected ||
      !hasLoadedWallets ||
      isNotEnoughDiskSpace ||
      !isSystemTimeCorrect ||
      displayAppUpdateOverlay
    ) {
      return react_1.default.createElement(LoadingPage_1.default, {
        stores: stores,
        actions: actions,
      });
    }
    if (!wallets.hasAnyWallets || isWalletImportDialogOpen) {
      return react_1.default.createElement(WalletAddPage_1.default, null);
    }
    return children;
  }
};
Root = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  Root
);
exports.default = Root;
//# sourceMappingURL=Root.js.map
