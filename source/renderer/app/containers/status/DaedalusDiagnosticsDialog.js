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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_modal_1 = __importDefault(require('react-modal'));
const DaedalusDiagnostics_1 = __importDefault(
  require('../../components/status/DaedalusDiagnostics')
);
const DaedalusDiagnosticsDialog_scss_1 = __importDefault(
  require('./DaedalusDiagnosticsDialog.scss')
);
const buildSystemInfo_1 = require('../../utils/buildSystemInfo');
let DaedalusDiagnosticsDialog = class DaedalusDiagnosticsDialog extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  handleForceCheckNetworkClock = () =>
    this.props.actions.networkStatus.forceCheckNetworkClock.trigger();
  handleCopyStateDirectoryPath = () =>
    this.props.actions.networkStatus.copyStateDirectoryPath.trigger();
  render() {
    const { actions, stores } = this.props;
    const { closeDaedalusDiagnosticsDialog } = actions.app;
    const { restartNode } = actions.networkStatus;
    const { app, networkStatus } = stores;
    const { openExternalLink } = app;
    const {
      // Node state
      cardanoNodeState,
      isNodeResponding,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
      // Application state
      isConnected,
      isSynced,
      syncPercentage,
      hasBeenConnected,
      localTimeDifference,
      isSystemTimeCorrect,
      isSystemTimeIgnored,
      openStateDirectory,
      getNetworkInfoRequest,
      networkTip,
      localTip,
      environment,
      tlsConfig,
      cardanoNodePID,
      cardanoWalletPID,
      stateDirectoryPath,
      getNetworkClockRequest,
    } = networkStatus;
    const systemInfo = (0, buildSystemInfo_1.buildSystemInfo)(
      environment,
      networkStatus
    );
    const {
      network,
      version,
      rendererProcessID,
      mainProcessID,
      isBlankScreenFixActive,
      nodeVersion,
      apiVersion,
      build,
    } = environment;
    const coreInfo = {
      daedalusVersion: version,
      daedalusBuildNumber: build,
      daedalusProcessID: rendererProcessID,
      daedalusMainProcessID: mainProcessID,
      daedalusStateDirectoryPath: stateDirectoryPath,
      isBlankScreenFixActive,
      cardanoNodeVersion: nodeVersion,
      cardanoNodePID,
      cardanoWalletVersion: apiVersion,
      cardanoWalletPID,
      cardanoWalletApiPort: tlsConfig ? tlsConfig.port : 0,
      cardanoNetwork: network,
    };
    return react_1.default.createElement(
      react_modal_1.default,
      {
        isOpen: true,
        closeOnOverlayClick: true,
        onRequestClose: closeDaedalusDiagnosticsDialog.trigger,
        className: DaedalusDiagnosticsDialog_scss_1.default.dialog,
        overlayClassName: DaedalusDiagnosticsDialog_scss_1.default.overlay,
        ariaHideApp: false,
      },
      react_1.default.createElement(DaedalusDiagnostics_1.default, {
        systemInfo: systemInfo,
        coreInfo: coreInfo,
        cardanoNodeState: cardanoNodeState,
        // @ts-ignore ts-migrate(2322) FIXME: Type '{ systemInfo: SystemInfo; coreInfo: { daedal... Remove this comment to see the full error message
        isDev: environment.isDev,
        isMainnet: environment.isMainnet,
        isStaging: environment.isStaging,
        isTestnet: environment.isTestnet,
        isNodeResponding: isNodeResponding,
        isNodeSyncing: isNodeSyncing,
        isNodeInSync: isNodeInSync,
        isNodeTimeCorrect: isNodeTimeCorrect,
        isConnected: isConnected,
        isSynced: isSynced,
        syncPercentage: syncPercentage,
        hasBeenConnected: hasBeenConnected,
        localTimeDifference: localTimeDifference,
        isSystemTimeCorrect: isSystemTimeCorrect,
        isSystemTimeIgnored: isSystemTimeIgnored,
        nodeConnectionError: getNetworkInfoRequest.error,
        localTip: localTip,
        networkTip: networkTip,
        isCheckingSystemTime:
          !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting,
        isForceCheckingSystemTime: getNetworkClockRequest.isExecutingWithArgs({
          isForceCheck: true,
        }),
        onOpenStateDirectory: openStateDirectory,
        onOpenExternalLink: openExternalLink,
        onRestartNode: restartNode,
        onClose: closeDaedalusDiagnosticsDialog.trigger,
        onCopyStateDirectoryPath: this.handleCopyStateDirectoryPath,
        onForceCheckNetworkClock: this.handleForceCheckNetworkClock,
      })
    );
  }
};
DaedalusDiagnosticsDialog = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  DaedalusDiagnosticsDialog
);
exports.default = DaedalusDiagnosticsDialog;
//# sourceMappingURL=DaedalusDiagnosticsDialog.js.map
