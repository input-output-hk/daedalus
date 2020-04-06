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

type State = {
  connectingTime: number,
};

type Props = {
  cardanoNodeState: ?CardanoNodeState,
  hasBeenConnected: boolean,
  forceConnectivityIssue?: boolean,
  isFlight: boolean,
  isConnected: boolean,
  isSynced: boolean,
  isConnecting: boolean,
  isSyncing: boolean,
  isSyncProgressStalling: boolean,
  isNodeStopping: boolean,
  isNodeStopped: boolean,
  isTlsCertInvalid: boolean,
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
  };

  componentDidMount() {
    this._defensivelyStartTimers(this.props.isConnected);
  }

  componentWillReceiveProps(nextProps: Props) {
    this._defensivelyStartTimers(nextProps.isConnected);
  }

  componentDidUpdate() {
    const { connectingTime } = this.state;
    const {
      isConnected,
      isSyncProgressStalling,
      onGetAvailableVersions,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
      isIncentivizedTestnet,
      isFlight,
    } = this.props;
    const canResetConnecting = this._connectingTimerShouldStop(isConnected);
    if (canResetConnecting) {
      this._resetConnectingTime();
    }
    const isAppLoadingStuck =
      isSyncProgressStalling ||
      (!isConnected && connectingTime >= REPORT_ISSUE_TIME_TRIGGER);
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
  }

  _connectingTimerShouldStart = (isConnected: boolean): boolean =>
    !isConnected && connectingInterval === null;

  _connectingTimerShouldStop = (isConnected: boolean): boolean =>
    isConnected && connectingInterval !== null;

  _defensivelyStartTimers = (isConnected: boolean) => {
    const needConnectingTimer = this._connectingTimerShouldStart(isConnected);
    if (needConnectingTimer) {
      connectingInterval = setInterval(this._incrementConnectingTime, 1000);
    }
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

  get showReportIssue() {
    const {
      isFlight,
      isConnected,
      isSyncProgressStalling,
      cardanoNodeState,
      isNewAppVersionLoaded,
      isNewAppVersionAvailable,
      isIncentivizedTestnet,
      forceConnectivityIssue,
    } = this.props;
    const { connectingTime } = this.state;
    const canReportConnectingIssue =
      isSyncProgressStalling ||
      forceConnectivityIssue ||
      (!isConnected &&
        (connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
          cardanoNodeState === CardanoNodeStates.UNRECOVERABLE));
    if (isFlight || isIncentivizedTestnet) {
      return canReportConnectingIssue;
    }
    return (
      isNewAppVersionLoaded &&
      !isNewAppVersionAvailable &&
      canReportConnectingIssue
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
              onIssueClick={onIssueClick}
              onOpenExternalLink={onOpenExternalLink}
              onDownloadLogs={onDownloadLogs}
              disableDownloadLogs={disableDownloadLogs}
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
