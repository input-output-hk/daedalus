// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SystemTimeErrorOverlay from './SystemTimeErrorOverlay';
import NoDiskSpaceOverlay from './NoDiskSpaceOverlay';
import ManualUpdateOverlay from './ManualUpdateOverlay';
import LoadingSpinner from '../widgets/LoadingSpinner';
import daedalusLogo from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
import linkNewWindow from '../../assets/images/link-ic.inline.svg';
import { CardanoNodeStates } from '../../../../common/types/cardano-node.types';
import styles from './Loading.scss';
import type { ReactIntlMessage } from '../../types/i18nTypes';
import type { CardanoNodeState } from '../../../../common/types/cardano-node.types';
import { REPORT_ISSUE_TIME_TRIGGER } from '../../config/timingConfig';

let connectingInterval = null;
let syncingInterval = null;

const messages = defineMessages({
  starting: {
    id: 'loading.screen.startingCardanoMessage',
    defaultMessage: '!!!Starting Cardano node',
    description: 'Message "Starting Cardano node" on the loading screen.'
  },
  stopping: {
    id: 'loading.screen.stoppingCardanoMessage',
    defaultMessage: '!!!Stopping Cardano node',
    description: 'Message "Stopping Cardano node" on the loading screen.'
  },
  stopped: {
    id: 'loading.screen.stoppedCardanoMessage',
    defaultMessage: '!!!Cardano node stopped',
    description: 'Message "Cardano node stopped" on the loading screen.'
  },
  updating: {
    id: 'loading.screen.updatingCardanoMessage',
    defaultMessage: '!!!Updating Cardano node',
    description: 'Message "Updating Cardano node" on the loading screen.'
  },
  updated: {
    id: 'loading.screen.updatedCardanoMessage',
    defaultMessage: '!!!Cardano node updated',
    description: 'Message "Cardano node updated" on the loading screen.'
  },
  crashed: {
    id: 'loading.screen.crashedCardanoMessage',
    defaultMessage: '!!!Cardano node crashed',
    description: 'Message "Cardano node crashed" on the loading screen.'
  },
  unrecoverable: {
    id: 'loading.screen.unrecoverableCardanoMessage',
    defaultMessage: '!!!Unable to start Cardano node. Please submit a support request.',
    description: 'Message "Unable to start Cardano node. Please submit a support request." on the loading screen.'
  },
  connecting: {
    id: 'loading.screen.connectingToNetworkMessage',
    defaultMessage: '!!!Connecting to network',
    description: 'Message "Connecting to network" on the loading screen.'
  },
  reconnecting: {
    id: 'loading.screen.reconnectingToNetworkMessage',
    defaultMessage: '!!!Network connection lost - reconnecting',
    description: 'Message "Network connection lost - reconnecting" on the loading screen.'
  },
  syncing: {
    id: 'loading.screen.syncingBlocksMessage',
    defaultMessage: '!!!Syncing blocks',
    description: 'Message "Syncing blocks" on the loading screen.'
  },
  reportConnectingIssueText: {
    id: 'loading.screen.reportIssue.connecting.text',
    defaultMessage: '!!!Having trouble connecting to network?',
    description: 'Report connecting issue text on the loading screen.'
  },
  reportSyncingIssueText: {
    id: 'loading.screen.reportIssue.syncing.text',
    defaultMessage: '!!!Having trouble syncing?',
    description: 'Report syncing issue text on the loading screen.'
  },
  reportIssueButtonLabel: {
    id: 'loading.screen.reportIssue.buttonLabel',
    defaultMessage: '!!!Open support ticket',
    description: 'Open support ticket button label on the loading.'
  },
  reportIssueDownloadLogsLinkLabel: {
    id: 'loading.screen.reportIssue.downloadLogsLinkLabel',
    defaultMessage: '!!!Download logs',
    description: 'Download logs button label on the loading.'
  },
});

type State = {
  connectingTime: number,
  syncingTime: number,
  syncPercentage: string,
};

type Props = {
  currencyIcon: string,
  apiIcon: string,
  cardanoNodeState: ?CardanoNodeState,
  hasBeenConnected: boolean,
  isConnected: boolean,
  isSynced: boolean,
  isNodeStopping: boolean,
  isNodeStopped: boolean,
  isNotEnoughDiskSpace: boolean,
  diskSpaceRequired: string,
  diskSpaceMissing: string,
  diskSpaceRecommended: string,
  syncPercentage: number,
  loadingDataForNextScreenMessage: ReactIntlMessage,
  hasLoadedCurrentLocale: boolean,
  hasLoadedCurrentTheme: boolean,
  localTimeDifference: ?number,
  isSystemTimeCorrect: boolean,
  isCheckingSystemTime: boolean,
  currentLocale: string,
  availableAppVersion: ?string,
  currentAppVersion: string,
  isNewAppVersionAvailable: boolean,
  isNewAppVersionLoading: boolean,
  onExternalLinkClick: Function,
  onReportIssueClick: Function,
  onCheckTheTimeAgain: Function,
  onContinueWithoutClockSyncCheck: Function,
  onDownloadLogs: Function,
  onGetAvailableVersions: Function,
};

@observer
export default class Loading extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    connectingTime: 0,
    syncingTime: 0,
    syncPercentage: '0',
  };

  appLoadingStuck = false;

  componentDidMount() {
    if (this.props.isNotEnoughDiskSpace) return;
    this._defensivelyStartTimers(this.props.isConnected, this.props.isSynced);
  }

  componentWillReceiveProps(nextProps: Props) {
    if (nextProps.isNotEnoughDiskSpace) return;
    this._defensivelyStartTimers(nextProps.isConnected, nextProps.isSynced);
  }

  componentDidUpdate() {
    const { syncingTime, connectingTime } = this.state;
    const {
      isConnected, isSynced, isNotEnoughDiskSpace,
      onGetAvailableVersions, isNewAppVersionLoading, availableAppVersion,
    } = this.props;
    const canResetSyncing = this._syncingTimerShouldStop(isSynced, isNotEnoughDiskSpace);
    const canResetConnecting = this._connectingTimerShouldStop(isConnected, isNotEnoughDiskSpace);
    if (canResetSyncing) { this._resetSyncingTime(); }
    if (canResetConnecting) { this._resetConnectingTime(); }

    this.appLoadingStuck = (
      (!isConnected && connectingTime >= REPORT_ISSUE_TIME_TRIGGER) ||
      (!isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER)
    );
    // If app stuck, check if newer version is available and set flag (state)
    if (this.appLoadingStuck && !isNewAppVersionLoading && !availableAppVersion) {
      onGetAvailableVersions();
    }
  }

  componentWillUnmount() {
    this._resetConnectingTime();
    this._resetSyncingTime();
  }

  _connectingTimerShouldStart = (isConnected: boolean): boolean => (
    !isConnected && connectingInterval === null
  );

  _syncingTimerShouldStart = (isConnected: boolean, isSynced: boolean): boolean => (
    isConnected && !isSynced && syncingInterval === null
  );

  _syncingTimerShouldStop = (
    isSynced: boolean, isNotEnoughDiskSpace: boolean
  ): boolean => (
    (isNotEnoughDiskSpace || isSynced) && syncingInterval !== null
  );

  _connectingTimerShouldStop = (
    isConnected: boolean, isNotEnoughDiskSpace: boolean
  ): boolean => (
    (isNotEnoughDiskSpace || isConnected) && connectingInterval !== null
  );

  _defensivelyStartTimers = (isConnected: boolean, isSynced: boolean) => {
    const needConnectingTimer = this._connectingTimerShouldStart(isConnected);
    const needSyncingTimer = this._syncingTimerShouldStart(isConnected, isSynced);
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
    this.setState({ connectingTime: this.state.connectingTime + 1 });
  };

  _incrementSyncingTime = () => {
    const syncPercentage = this.props.syncPercentage.toFixed(2);
    if (syncPercentage === this.state.syncPercentage) {
      // syncPercentage not increased, increase syncing time
      this.setState({ syncingTime: this.state.syncingTime + 1 });
    } else {
      // reset syncingTime and set new max percentage
      this.setState({ syncingTime: 0, syncPercentage });
    }
  };

  _getConnectingMessage = () => {
    const { cardanoNodeState, hasBeenConnected } = this.props;
    let connectingMessage;
    switch (cardanoNodeState) {
      case null:
      case CardanoNodeStates.STARTING:
        connectingMessage = messages.starting;
        break;
      case CardanoNodeStates.STOPPING:
      case CardanoNodeStates.EXITING:
        connectingMessage = messages.stopping;
        break;
      case CardanoNodeStates.STOPPED:
        connectingMessage = messages.stopped;
        break;
      case CardanoNodeStates.UPDATING:
        connectingMessage = messages.updating;
        break;
      case CardanoNodeStates.UPDATED:
        connectingMessage = messages.updated;
        break;
      case CardanoNodeStates.CRASHED:
      case CardanoNodeStates.ERRORED:
        connectingMessage = messages.crashed;
        break;
      case CardanoNodeStates.UNRECOVERABLE:
        connectingMessage = messages.unrecoverable;
        break;
      default: // also covers CardanoNodeStates.RUNNING state
        connectingMessage = hasBeenConnected ? messages.reconnecting : messages.connecting;
    }
    return connectingMessage;
  };

  _renderLoadingScreen = () => {
    const { intl } = this.context;
    const {
      isConnected,
      isSystemTimeCorrect,
      isSynced,
      isNodeStopping,
      isNodeStopped,
      isNotEnoughDiskSpace,
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
      localTimeDifference,
      currentLocale,
      onExternalLinkClick,
      onCheckTheTimeAgain,
      onContinueWithoutClockSyncCheck,
      isCheckingSystemTime,
      syncPercentage,
      loadingDataForNextScreenMessage,
    } = this.props;

    if (isNotEnoughDiskSpace) {
      return (
        <NoDiskSpaceOverlay
          diskSpaceRequired={diskSpaceRequired}
          diskSpaceMissing={diskSpaceMissing}
          diskSpaceRecommended={diskSpaceRecommended}
        />
      );
    }

    if (!isSystemTimeCorrect && !isNodeStopping && !isNodeStopped) {
      return (
        <SystemTimeErrorOverlay
          localTimeDifference={localTimeDifference}
          currentLocale={currentLocale}
          onExternalLinkClick={onExternalLinkClick}
          onCheckTheTimeAgain={onCheckTheTimeAgain}
          onContinueWithoutClockSyncCheck={onContinueWithoutClockSyncCheck}
          isCheckingSystemTime={isCheckingSystemTime}
        />
      );
    }

    if (!isConnected) {
      const headlineClasses = classNames([
        styles.headline,
        isNodeStopped ? styles.withoutAnimation : null,
      ]);
      return (
        <div className={styles.connecting}>
          <h1 className={headlineClasses}>
            {intl.formatMessage(this._getConnectingMessage())}
          </h1>
        </div>
      );
    }

    if (!isSynced) {
      return (
        <div className={styles.syncing}>
          <h1 className={styles.headline}>
            {intl.formatMessage(messages.syncing)} {syncPercentage.toFixed(2)}%
          </h1>
        </div>
      );
    }

    return (
      <div className={styles.syncing}>
        <div>
          <h1 className={styles.headline}>
            {intl.formatMessage(loadingDataForNextScreenMessage)}
          </h1>
          <LoadingSpinner />
        </div>
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const {
      cardanoNodeState,
      currencyIcon,
      apiIcon,
      isConnected,
      isSynced,
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      onReportIssueClick,
      onDownloadLogs,
      isNewAppVersionAvailable,
      currentAppVersion,
      availableAppVersion,
      isNewAppVersionLoading,
      onExternalLinkClick,
    } = this.props;

    const { connectingTime, syncingTime } = this.state;

    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles['is-loading-theme'],
      !isConnected ? styles['is-connecting'] : null,
      isConnected && !isSynced ? styles['is-syncing'] : null,
    ]);
    const daedalusLogoStyles = classNames([
      styles.daedalusLogo,
      !isConnected ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const currencyLogoStyles = classNames([
      styles['ada-logo'],
      !isConnected ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const apiLogoStyles = classNames([
      styles['ada-apiLogo'],
      !isConnected ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const downloadLogsButtonStyles = classNames([
      styles.downloadLogsButton,
      !isConnected ? styles.downloadLogsButtonConnecting : null,
    ]);

    const daedalusLoadingLogo = daedalusLogo;
    const currencyLoadingLogo = currencyIcon;
    const apiLoadingLogo = apiIcon;

    const canReportConnectingIssue = (
      !isConnected && (
        connectingTime >= REPORT_ISSUE_TIME_TRIGGER ||
        cardanoNodeState === CardanoNodeStates.UNRECOVERABLE
      )
    );
    const canReportSyncingIssue = (
      isConnected && !isSynced && syncingTime >= REPORT_ISSUE_TIME_TRIGGER
    );

    const showReportIssue = (
      !isNewAppVersionAvailable && !isNewAppVersionLoading &&
      (canReportConnectingIssue || canReportSyncingIssue)
    );

    const buttonClasses = classNames([
      'primary',
      styles.reportIssueButton,
    ]);

    const isManualUpdateRequired = isNewAppVersionAvailable && this.appLoadingStuck;

    return (
      <div className={componentStyles}>
        {showReportIssue && (
          <div className={styles.reportIssue}>
            <h1 className={styles.reportIssueText}>
              {!isConnected ?
                intl.formatMessage(messages.reportConnectingIssueText) :
                intl.formatMessage(messages.reportSyncingIssueText)
              }
            </h1>
            <Button
              className={buttonClasses}
              label={(
                <p>
                  <SVGInline svg={linkNewWindow} className={styles.linkNewWindow} />
                  {intl.formatMessage(messages.reportIssueButtonLabel)}
                </p>
              )}
              onClick={onReportIssueClick}
              skin={ButtonSkin}
            />
            <br />
            <button
              className={downloadLogsButtonStyles}
              onClick={onDownloadLogs}
            >
              {intl.formatMessage(messages.reportIssueDownloadLogsLinkLabel)}
            </button>
          </div>
        )}
        <div className={styles.logos}>
          <SVGInline svg={currencyLoadingLogo} className={currencyLogoStyles} />
          <SVGInline svg={daedalusLoadingLogo} className={daedalusLogoStyles} />
          <SVGInline svg={apiLoadingLogo} className={apiLogoStyles} />
        </div>
        {hasLoadedCurrentLocale ? this._renderLoadingScreen() : null}
        {isManualUpdateRequired && (
          <ManualUpdateOverlay
            currentAppVersion={currentAppVersion}
            availableAppVersion={availableAppVersion}
            onExternalLinkClick={onExternalLinkClick}
          />
        )}
      </div>
    );
  }

}
