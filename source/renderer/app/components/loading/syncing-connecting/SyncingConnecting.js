// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import StatusIcons from './StatusIcons';
import ReportIssue from './ReportIssue';
import LogosDisplay from './LogosDisplay';
import SyncingConnectingTitle from './SyncingConnectingTitle';
import { CardanoNodeStates } from '../../../../../common/types/cardano-node.types';
import styles from './SyncingConnecting.scss';
import type { CardanoNodeState } from '../../../../../common/types/cardano-node.types';
import { REPORT_ISSUE_TIME_TRIGGER } from '../../../config/timingConfig';

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
  isConnected: boolean,
  isSynced: boolean,
  isConnecting: boolean,
  isSyncing: boolean,
  isNodeStopping: boolean,
  isNodeStopped: boolean,
  isTlsCertInvalid: boolean,
  syncPercentage: number,
  hasLoadedCurrentLocale: boolean,
  hasLoadedCurrentTheme: boolean,
  isCheckingSystemTime: boolean,
  isNodeResponding: boolean,
  isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeTimeCorrect: boolean,
  isNewAppVersionAvailable: boolean,
  isNewAppVersionLoading: boolean,
  isNewAppVersionLoaded: boolean,
  onReportIssueClick: Function,
  onDownloadLogs: Function,
  onGetAvailableVersions: Function,
  disableDownloadLogs: boolean,
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
      onGetAvailableVersions,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
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
      (!isConnected && connectingTime >= REPORT_ISSUE_TIME_TRIGGER) ||
      (isConnected && !isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER);
    // If app loading is stuck, check if a newer version is available and set flag (state)
    if (
      isAppLoadingStuck &&
      !isNewAppVersionLoaded &&
      !isNewAppVersionLoading
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

  render() {
    const {
      cardanoNodeState,
      isConnected,
      isSynced,
      isConnecting,
      isSyncing,
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      onReportIssueClick,
      onDownloadLogs,
      disableDownloadLogs,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeTimeCorrect,
      isCheckingSystemTime,
      isNewAppVersionAvailable,
      isNewAppVersionLoaded,
      hasBeenConnected,
      isTlsCertInvalid,
      isNodeStopping,
      isNodeStopped,
      syncPercentage,
    } = this.props;

    const { connectingTime, syncingTime } = this.state;

    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles['is-loading-theme'],
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);

    const canReportConnectingIssue =
      !isConnected &&
      (connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
        cardanoNodeState === CardanoNodeStates.UNRECOVERABLE);
    const canReportSyncingIssue =
      isConnected && !isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER;
    const showReportIssue =
      isNewAppVersionLoaded &&
      !isNewAppVersionAvailable &&
      (canReportConnectingIssue || canReportSyncingIssue);

    return (
      <div className={componentStyles}>
        {showReportIssue && (
          <ReportIssue
            isConnected={isConnected}
            onReportIssueClick={onReportIssueClick}
            onDownloadLogs={onDownloadLogs}
            disableDownloadLogs={disableDownloadLogs}
            isConnecting={isConnecting}
            isSyncing={isSyncing}
          />
        )}
        <LogosDisplay isConnected={isConnected} />
        <SyncingConnectingTitle
          cardanoNodeState={cardanoNodeState}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasBeenConnected={hasBeenConnected}
          isTlsCertInvalid={isTlsCertInvalid}
          isConnected={isConnected}
          isSynced={isSynced}
          isNodeStopping={isNodeStopping}
          isNodeStopped={isNodeStopped}
          syncPercentage={syncPercentage}
        />
        <StatusIcons
          nodeState={cardanoNodeState}
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeTimeCorrect={
            isCheckingSystemTime ? undefined : isNodeTimeCorrect
          }
          isNodeSyncing={isNodeSyncing}
          isCheckingSystemTime={isCheckingSystemTime}
        />
      </div>
    );
  }
}
