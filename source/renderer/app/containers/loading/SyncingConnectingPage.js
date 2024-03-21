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
const SyncingConnecting_1 = __importDefault(
  require('../../components/loading/syncing-connecting/SyncingConnecting')
);
const reporting_1 = require('../../../../common/utils/reporting');
let LoadingSyncingConnectingPage = class LoadingSyncingConnectingPage extends react_1.Component {
  static defaultProps = {
    stores: null,
    actions: null,
  };
  render() {
    const {
      newsFeed,
      appUpdate,
      networkStatus,
      profile,
      app,
    } = this.props.stores;
    const {
      cardanoNodeState,
      isNodeResponding,
      isNodeSyncing,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      isSyncProgressStalling,
      hasBeenConnected,
      getNetworkClockRequest,
      isNodeStopping,
      isNodeStopped,
      isNotEnoughDiskSpace,
      isTlsCertInvalid,
      isVerifyingBlockchain,
      blockSyncProgress,
    } = networkStatus;
    const { displayAppUpdateNewsItem } = appUpdate;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = profile;
    const { toggleNewsFeed } = this.props.actions.app;
    const { unread } = newsFeed.newsFeedData;
    const hasNotification = unread.length > 0;
    return react_1.default.createElement(SyncingConnecting_1.default, {
      cardanoNodeState: cardanoNodeState,
      hasBeenConnected: hasBeenConnected,
      isConnected: isConnected,
      isSynced: isSynced,
      isConnecting: !isConnected,
      isSyncing: isConnected && !isSynced,
      isSyncProgressStalling: isSyncProgressStalling,
      isNodeStopping: isNodeStopping,
      isNodeStopped: isNodeStopped,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isNotEnoughDiskSpace: isNotEnoughDiskSpace,
      isTlsCertInvalid: isTlsCertInvalid,
      hasNotification: hasNotification,
      hasUpdate: displayAppUpdateNewsItem,
      hasLoadedCurrentLocale: hasLoadedCurrentLocale,
      hasLoadedCurrentTheme: hasLoadedCurrentTheme,
      isCheckingSystemTime:
        !getNetworkClockRequest.result || getNetworkClockRequest.isExecuting,
      isNodeResponding: isNodeResponding,
      isNodeSyncing: isNodeSyncing,
      isNodeTimeCorrect: isNodeTimeCorrect,
      onIssueClick: this.handleIssueClick,
      onOpenExternalLink: this.handleOpenExternalLink,
      onStatusIconClick: this.openDaedalusDiagnosticsDialog,
      onDownloadLogs: this.handleDownloadLogs,
      onToggleNewsFeedIconClick: toggleNewsFeed.trigger,
      disableDownloadLogs: app.isDownloadNotificationVisible,
      showNewsFeedIcon: !isNodeStopping && !isNodeStopped,
      isVerifyingBlockchain: isVerifyingBlockchain,
      blockSyncProgress: blockSyncProgress,
    });
  }
  handleIssueClick = async (issueButtonUrl) => {
    const locale = this.props.stores.profile.currentLocale;
    const { environment } = this.props.stores.app;
    const supportUrl = (0, reporting_1.generateSupportRequestLink)(
      issueButtonUrl,
      environment,
      locale
    );
    this.props.stores.app.openExternalLink(supportUrl);
  };
  handleOpenExternalLink = (articleUrl) => {
    this.props.stores.app.openExternalLink(articleUrl);
  };
  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setIsDownloadingLogs.trigger(true);
  };
  openDaedalusDiagnosticsDialog = () => {
    const {
      actions: { app },
    } = this.props;
    app.openDaedalusDiagnosticsDialog.trigger();
  };
};
LoadingSyncingConnectingPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  LoadingSyncingConnectingPage
);
exports.default = LoadingSyncingConnectingPage;
//# sourceMappingURL=SyncingConnectingPage.js.map
