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
const classnames_1 = __importDefault(require('classnames'));
const StatusIcons_1 = __importDefault(require('./StatusIcons'));
const ReportIssue_1 = __importDefault(require('./ReportIssue'));
const LogosDisplay_1 = __importDefault(require('./LogosDisplay'));
const SyncingConnectingBackground_1 = __importDefault(
  require('./SyncingConnectingBackground')
);
const SyncingConnectingStatus_1 = __importDefault(
  require('./SyncingConnectingStatus')
);
const cardano_node_types_1 = require('../../../../../common/types/cardano-node.types');
const SyncingConnecting_scss_1 = __importDefault(
  require('./SyncingConnecting.scss')
);
const timingConfig_1 = require('../../../config/timingConfig');
const NewsFeedIcon_1 = __importDefault(require('../../widgets/NewsFeedIcon'));
let connectingInterval = null;
let SyncingConnecting = class SyncingConnecting extends react_1.Component {
  state = {
    connectingTime: 0,
  };
  componentDidMount() {
    const { isConnected, isVerifyingBlockchain } = this.props;
    this._defensivelyStartTimers(isConnected, isVerifyingBlockchain);
  }
  componentDidUpdate() {
    const { isConnected, isVerifyingBlockchain } = this.props;
    const canResetConnecting = this._connectingTimerShouldStop(
      isConnected,
      isVerifyingBlockchain
    );
    this._defensivelyStartTimers(isConnected, isVerifyingBlockchain);
    if (canResetConnecting) {
      this._resetConnectingTime();
    }
  }
  componentWillUnmount() {
    this._resetConnectingTime();
  }
  _connectingTimerShouldStart = (isConnected, isVerifyingBlockchain) =>
    !isConnected && !isVerifyingBlockchain && connectingInterval === null;
  _connectingTimerShouldStop = (isConnected, isVerifyingBlockchain) =>
    (isConnected || isVerifyingBlockchain) && connectingInterval !== null;
  _defensivelyStartTimers = (isConnected, isVerifyingBlockchain) => {
    const needConnectingTimer = this._connectingTimerShouldStart(
      isConnected,
      isVerifyingBlockchain
    );
    if (needConnectingTimer) {
      connectingInterval = setInterval(this._incrementConnectingTime, 1000);
    }
  };
  _resetConnectingTime = () => {
    if (connectingInterval !== null) {
      clearInterval(connectingInterval);
      connectingInterval = null;
    }
    this.setState({
      connectingTime: 0,
    });
  };
  _incrementConnectingTime = () => {
    this.setState((prevState) => ({
      connectingTime: prevState.connectingTime + 1,
    }));
  };
  get showReportIssue() {
    const {
      isConnected,
      isSyncProgressStalling,
      cardanoNodeState,
      forceConnectivityIssue,
      isVerifyingBlockchain,
    } = this.props;
    const { connectingTime } = this.state;
    const canReportConnectingIssue =
      !isVerifyingBlockchain &&
      (isSyncProgressStalling ||
        forceConnectivityIssue ||
        (!isConnected &&
          (connectingTime >= timingConfig_1.REPORT_ISSUE_TIME_TRIGGER ||
            cardanoNodeState ===
              cardano_node_types_1.CardanoNodeStates.UNRECOVERABLE)));
    return canReportConnectingIssue;
  }
  render() {
    const {
      cardanoNodeState,
      isConnected,
      isSynced,
      isConnecting,
      isSyncing,
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      hasNotification,
      hasUpdate,
      onIssueClick,
      onOpenExternalLink,
      onDownloadLogs,
      disableDownloadLogs,
      isNodeResponding,
      isNodeSyncing,
      isNodeTimeCorrect,
      isCheckingSystemTime,
      hasBeenConnected,
      isTlsCertInvalid,
      isNodeStopping,
      isNodeStopped,
      onStatusIconClick,
      onToggleNewsFeedIconClick,
      showNewsFeedIcon,
      isVerifyingBlockchain,
      blockSyncProgress,
    } = this.props;
    const newsFeedIconStyles = (0, classnames_1.default)([
      isConnecting ? 'connectingScreen' : null,
      isSyncing || isSynced ? 'syncingScreen' : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: SyncingConnecting_scss_1.default.component },
      react_1.default.createElement(SyncingConnectingBackground_1.default, {
        hasLoadedCurrentTheme: hasLoadedCurrentTheme,
        isConnecting: isConnecting,
        isSyncing: isSyncing,
      }),
      react_1.default.createElement(
        'div',
        { className: SyncingConnecting_scss_1.default.content },
        this.showReportIssue &&
          react_1.default.createElement(ReportIssue_1.default, {
            onIssueClick: onIssueClick,
            onOpenExternalLink: onOpenExternalLink,
            onDownloadLogs: onDownloadLogs,
            disableDownloadLogs: disableDownloadLogs,
          }),
        showNewsFeedIcon &&
          react_1.default.createElement(NewsFeedIcon_1.default, {
            onNewsFeedIconClick: onToggleNewsFeedIconClick,
            newsFeedIconClass: newsFeedIconStyles,
            hasNotification: hasNotification,
            hasUpdate: hasUpdate,
          }),
        react_1.default.createElement(LogosDisplay_1.default, {
          isConnected: isConnected,
        })
      ),
      react_1.default.createElement(SyncingConnectingStatus_1.default, {
        cardanoNodeState: cardanoNodeState,
        hasLoadedCurrentLocale: hasLoadedCurrentLocale,
        hasBeenConnected: hasBeenConnected,
        isTlsCertInvalid: isTlsCertInvalid,
        isConnected: isConnected,
        isNodeStopping: isNodeStopping,
        isNodeStopped: isNodeStopped,
        isVerifyingBlockchain: isVerifyingBlockchain,
        blockSyncProgress: blockSyncProgress,
      }),
      react_1.default.createElement(StatusIcons_1.default, {
        onIconClick: onStatusIconClick,
        nodeState: cardanoNodeState,
        isNodeResponding: isNodeResponding,
        isNodeTimeCorrect: isCheckingSystemTime ? undefined : isNodeTimeCorrect,
        isNodeSyncing: isNodeSyncing,
      })
    );
  }
};
SyncingConnecting = __decorate([mobx_react_1.observer], SyncingConnecting);
exports.default = SyncingConnecting;
//# sourceMappingURL=SyncingConnecting.js.map
