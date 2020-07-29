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
  verificationProgress: number,
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
  isVerifyingBlockchain: boolean,
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
    const { isConnected, isVerifyingBlockchain } = this.props;
    this._defensivelyStartTimers(isConnected, isVerifyingBlockchain);
  }

  componentDidUpdate() {
    const { connectingTime } = this.state;
    const {
      isConnected,
      cardanoNodeState,
      isSyncProgressStalling,
      onGetAvailableVersions,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
      isIncentivizedTestnet,
      isFlight,
      isVerifyingBlockchain,
    } = this.props;
    const canResetConnecting = this._connectingTimerShouldStop(
      isConnected,
      isVerifyingBlockchain
    );

    this._defensivelyStartTimers(isConnected, isVerifyingBlockchain);
    if (canResetConnecting) {
      this._resetConnectingTime();
    }
    const isAppLoadingStuck =
      !isVerifyingBlockchain &&
      (isSyncProgressStalling ||
        (!isConnected &&
          (connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
            cardanoNodeState === CardanoNodeStates.UNRECOVERABLE)));
    // If app loading is stuck, check if a newer version is available and set flag (state)
    if (
      isAppLoadingStuck &&
      !isNewAppVersionLoaded &&
      !isNewAppVersionLoading &&
      !isIncentivizedTestnet &&
      !global.isShelleyTestnet &&
      !isFlight
    ) {
      onGetAvailableVersions();
    }
  }

  componentWillUnmount() {
    this._resetConnectingTime();
  }

  _connectingTimerShouldStart = (
    isConnected: boolean,
    isVerifyingBlockchain: boolean
  ): boolean =>
    !isConnected && !isVerifyingBlockchain && connectingInterval === null;

  _connectingTimerShouldStop = (
    isConnected: boolean,
    isVerifyingBlockchain: boolean
  ): boolean =>
    (isConnected || isVerifyingBlockchain) && connectingInterval !== null;

  _defensivelyStartTimers = (
    isConnected: boolean,
    isVerifyingBlockchain: boolean
  ) => {
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
      isVerifyingBlockchain,
    } = this.props;
    const { connectingTime } = this.state;
    const canReportConnectingIssue =
      !isVerifyingBlockchain &&
      (isSyncProgressStalling ||
        forceConnectivityIssue ||
        (!isConnected &&
          (connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
            cardanoNodeState === CardanoNodeStates.UNRECOVERABLE)));

    if (isFlight || isIncentivizedTestnet || global.isShelleyTestnet) {
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
      isVerifyingBlockchain,
      verificationProgress,
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
          isVerifyingBlockchain={isVerifyingBlockchain}
          verificationProgress={verificationProgress}
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
