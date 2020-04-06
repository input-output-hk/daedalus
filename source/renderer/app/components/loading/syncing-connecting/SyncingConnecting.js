// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import StatusIcons from './StatusIcons';
import ReportIssue from './ReportIssue';
import LogosDisplay from './LogosDisplay';
import SyncingConnectingBackground from './SyncingConnectingBackground';
import SyncingConnectingTitle from './SyncingConnectingTitle';
import SyncingConnectingStatus from './SyncingConnectingStatus';
import { CardanoNodeStates } from '../../../../../common/types/cardano-node.types';
import styles from './SyncingConnecting.scss';
import type { CardanoNodeState } from '../../../../../common/types/cardano-node.types';
import { REPORT_ISSUE_TIME_TRIGGER } from '../../../config/timingConfig';
import NewsFeedIcon from '../../widgets/NewsFeedIcon';

let connectingInterval = null;
let syncingInterval = null;

type State = {
  connectingTime: number,
  syncingTime: number,
  syncPercentage: string,
};

type Props = {
  cardanoNodeState: ?CardanoNodeState,
  hasBeenConnected: boolean,
  forceConnectivityIssue?: boolean,
  forceSyncIssue?: boolean,
  isFlight: boolean,
  isConnected: boolean,
  isSynced: boolean,
  isConnecting: boolean,
  isSyncing: boolean,
  isSyncProgressStalling: boolean,
  isNodeStopping: boolean,
  isNodeStopped: boolean,
  isTlsCertInvalid: boolean,
  syncPercentage: number,
  hasLoadedCurrentLocale: boolean,
  hasLoadedCurrentTheme: boolean,
  hasUnreadNews: boolean,
  isCheckingSystemTime: boolean,
  isNodeResponding: boolean,
  isNodeSyncing: boolean,
  isNodeTimeCorrect: boolean,
  isNewAppVersionAvailable: boolean,
  isNewAppVersionLoading: boolean,
  isNewAppVersionLoaded: boolean,
  disableDownloadLogs: boolean,
  showNewsFeedIcon: boolean,
  isIncentivizedTestnet: boolean,
  onIssueClick: Function,
  onOpenExternalLink: Function,
  onDownloadLogs: Function,
  onGetAvailableVersions: Function,
  onStatusIconClick: Function,
  onToggleNewsFeedIconClick: Function,
};

@observer
export default class SyncingConnecting extends Component<Props, State> {
  state = {
    connectingTime: 0,
    syncingTime: 0,
    syncPercentage: '0',
  };

  componentDidMount() {
    this._defensivelyStartTimers(this.props.isConnected, this.props.isSynced);
  }

  componentWillReceiveProps(nextProps: Props) {
    this._defensivelyStartTimers(nextProps.isConnected, nextProps.isSynced);
  }

  componentDidUpdate() {
    const { syncingTime, connectingTime } = this.state;
    const {
      isConnected,
      isSynced,
      isSyncProgressStalling,
      onGetAvailableVersions,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
      isIncentivizedTestnet,
      isFlight,
    } = this.props;
    const canResetSyncing = this._syncingTimerShouldStop(isSynced);
    const canResetConnecting = this._connectingTimerShouldStop(isConnected);
    if (canResetSyncing) {
      this._resetSyncingTime();
    }
    if (canResetConnecting) {
      this._resetConnectingTime();
    }
    const isAppLoadingStuck =
      isSyncProgressStalling ||
      (!isConnected && connectingTime >= REPORT_ISSUE_TIME_TRIGGER) ||
      (isConnected && !isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER);
    // If app loading is stuck, check if a newer version is available and set flag (state)
    if (
      isAppLoadingStuck &&
      !isNewAppVersionLoaded &&
      !isNewAppVersionLoading &&
      !isIncentivizedTestnet &&
      !isFlight
    ) {
      onGetAvailableVersions();
    }
  }

  componentWillUnmount() {
    this._resetConnectingTime();
    this._resetSyncingTime();
  }

  _connectingTimerShouldStart = (isConnected: boolean): boolean =>
    !isConnected && connectingInterval === null;

  _syncingTimerShouldStart = (
    isConnected: boolean,
    isSynced: boolean
  ): boolean => isConnected && !isSynced && syncingInterval === null;

  _syncingTimerShouldStop = (isSynced: boolean): boolean =>
    isSynced && syncingInterval !== null;

  _connectingTimerShouldStop = (isConnected: boolean): boolean =>
    isConnected && connectingInterval !== null;

  _defensivelyStartTimers = (isConnected: boolean, isSynced: boolean) => {
    const needConnectingTimer = this._connectingTimerShouldStart(isConnected);
    const needSyncingTimer = this._syncingTimerShouldStart(
      isConnected,
      isSynced
    );
    if (needConnectingTimer) {
      connectingInterval = setInterval(this._incrementConnectingTime, 1000);
    } else if (needSyncingTimer) {
      syncingInterval = setInterval(this._incrementSyncingTime, 1000);
    }
  };

  _resetSyncingTime = () => {
    if (syncingInterval !== null) {
      clearInterval(syncingInterval);
      syncingInterval = null;
    }
    this.setState({ syncingTime: 0 });
  };

  _resetConnectingTime = () => {
    if (connectingInterval !== null) {
      clearInterval(connectingInterval);
      connectingInterval = null;
    }
    this.setState({ connectingTime: 0 });
  };

  _incrementConnectingTime = () => {
    this.setState(prevState => ({
      connectingTime: prevState.connectingTime + 1,
    }));
  };

  _incrementSyncingTime = () => {
    const syncPercentage = this.props.syncPercentage.toFixed(2);
    if (syncPercentage === this.state.syncPercentage) {
      // syncPercentage not increased, increase syncing time
      this.setState(prevState => ({ syncingTime: prevState.syncingTime + 1 }));
    } else {
      // reset syncingTime and set new max percentage
      this.setState({ syncingTime: 0, syncPercentage });
    }
  };

  get showReportIssue() {
    const {
      isFlight,
      isConnected,
      isSynced,
      isSyncProgressStalling,
      cardanoNodeState,
      isNewAppVersionLoaded,
      isNewAppVersionAvailable,
      isIncentivizedTestnet,
      forceConnectivityIssue,
      forceSyncIssue,
    } = this.props;
    const { connectingTime, syncingTime } = this.state;
    const canReportConnectingIssue =
      isSyncProgressStalling ||
      forceConnectivityIssue ||
      (!isConnected &&
        (connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
          cardanoNodeState === CardanoNodeStates.UNRECOVERABLE));
    const canReportSyncingIssue =
      forceSyncIssue ||
      (isConnected && !isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER);

    if (isFlight || isIncentivizedTestnet) {
      return canReportConnectingIssue || canReportSyncingIssue;
    }

    return (
      isNewAppVersionLoaded &&
      !isNewAppVersionAvailable &&
      (canReportConnectingIssue || canReportSyncingIssue)
    );
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
      hasUnreadNews,
      onIssueClick,
      onOpenExternalLink,
      onDownloadLogs,
      disableDownloadLogs,
      isIncentivizedTestnet,
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
    } = this.props;

    const newsFeedIconStyles = classNames([
      isConnecting ? 'connectingScreen' : null,
      isSyncing || isSynced ? 'syncingScreen' : null,
    ]);

    return (
      <div className={styles.component}>
        <SyncingConnectingBackground
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          isIncentivizedTestnet={isIncentivizedTestnet}
          isConnecting={isConnecting}
          isSyncing={isSyncing}
        />
        <div className={styles.content}>
          {this.showReportIssue && (
            <ReportIssue
              isConnected={isConnected}
              onIssueClick={onIssueClick}
              onOpenExternalLink={onOpenExternalLink}
              onDownloadLogs={onDownloadLogs}
              disableDownloadLogs={disableDownloadLogs}
              isConnecting={isConnecting}
              isSyncing={isSyncing}
              isIncentivizedTestnet={isIncentivizedTestnet}
            />
          )}
          {showNewsFeedIcon && (
            <NewsFeedIcon
              onNewsFeedIconClick={onToggleNewsFeedIconClick}
              newsFeedIconClass={newsFeedIconStyles}
              showDot={hasUnreadNews}
            />
          )}
          <LogosDisplay isConnected={isConnected} />
          {isIncentivizedTestnet && <SyncingConnectingTitle />}
        </div>
        <SyncingConnectingStatus
          cardanoNodeState={cardanoNodeState}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasBeenConnected={hasBeenConnected}
          isTlsCertInvalid={isTlsCertInvalid}
          isConnected={isConnected}
          isNodeStopping={isNodeStopping}
          isNodeStopped={isNodeStopped}
        />
        <StatusIcons
          onIconClick={onStatusIconClick}
          nodeState={cardanoNodeState}
          isNodeResponding={isNodeResponding}
          isNodeTimeCorrect={
            isCheckingSystemTime ? undefined : isNodeTimeCorrect
          }
          isNodeSyncing={isNodeSyncing}
        />
      </div>
    );
  }
}
