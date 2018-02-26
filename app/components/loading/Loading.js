// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/raw/ButtonSkin';
import SystemTimeErrorOverlay from './SystemTimeErrorOverlay';
import LoadingSpinner from '../widgets/LoadingSpinner';
import daedalusLogo from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
import styles from './Loading.scss';
import type { ReactIntlMessage } from '../../types/i18nTypes';
import environment from '../../environment';

const REPORT_ISSUE_TIME_TRIGGER = 5 * 60; // 5 minutes
let connectingInterval = null;
let syncingInterval = null;

const messages = defineMessages({
  connecting: {
    id: 'loading.screen.connectingToNetworkMessage',
    defaultMessage: '!!!Connecting to network',
    description: 'Message "Connecting to network" on the loading screen.'
  },
  waitingForSyncToStart: {
    id: 'loading.screen.waitingForSyncToStart',
    defaultMessage: '!!!Connected - waiting for block syncing to start',
    description: 'Message "Connected - waiting for block syncing to start" on the loading screen.'
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
    defaultMessage: '!!!Report an issue',
    description: 'Report an issue button label on the loading .'
  }
});

type State = {
  connectingTime: number,
  syncingTime: number,
  syncPercentage: string,
};

type Props = {
  currencyIcon: string,
  apiIcon: string,
  isConnecting: boolean,
  hasBeenConnected: boolean,
  hasBlockSyncingStarted: boolean,
  isSyncing: boolean,
  syncPercentage: number,
  isLoadingDataForNextScreen: boolean,
  loadingDataForNextScreenMessage: ReactIntlMessage,
  hasLoadedCurrentLocale: boolean,
  hasLoadedCurrentTheme: boolean,
  localTimeDifference: number,
  allowedTimeDifference: number,
  currentLocale: string,
  handleReportIssue: Function,
  onProblemSolutionClick: Function,
};

@observer
export default class Loading extends Component<Props, State> {
  constructor() {
    super();
    this.state = {
      connectingTime: 0,
      syncingTime: 0,
      syncPercentage: '0',
    };
  }

  componentWillMount() {
    if (this.props.isConnecting) {
      connectingInterval = setInterval(this.connectingTimer, 1000);
    }

    if (this.props.isSyncing) {
      syncingInterval = setInterval(this.syncingTimer, 1000);
    }
  }

  componentWillUnmount() {
    if (connectingInterval) {
      clearInterval(connectingInterval);
    }

    if (syncingInterval) {
      clearInterval(syncingInterval);
    }
  }

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      currencyIcon,
      apiIcon,
      isConnecting,
      isSyncing,
      syncPercentage,
      isLoadingDataForNextScreen,
      loadingDataForNextScreenMessage,
      hasBeenConnected,
      hasBlockSyncingStarted,
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      localTimeDifference,
      allowedTimeDifference,
      currentLocale,
      handleReportIssue,
      onProblemSolutionClick,
    } = this.props;

    const { connectingTime, syncingTime } = this.state;

    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles['is-loading-theme'],
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);
    const daedalusLogoStyles = classNames([
      styles.daedalusLogo,
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const currencyLogoStyles = classNames([
      styles[`${environment.API}-logo`],
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const apiLogoStyles = classNames([
      styles[`${environment.API}-apiLogo`],
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);

    const daedalusLoadingLogo = daedalusLogo;
    const currencyLoadingLogo = currencyIcon;
    const apiLoadingLogo = apiIcon;

    let connectingMessage;
    if (hasBeenConnected) {
      connectingMessage = messages.reconnecting;
    } else {
      connectingMessage = (
        hasBlockSyncingStarted ? messages.waitingForSyncToStart : messages.connecting
      );
    }

    const canReportConnectingIssue = isConnecting && connectingTime >= REPORT_ISSUE_TIME_TRIGGER;
    const canReportSyncingIssue = isSyncing && syncingTime >= REPORT_ISSUE_TIME_TRIGGER;
    const showReportIssue = canReportConnectingIssue || canReportSyncingIssue;

    const buttonClasses = classNames([
      'primary',
      styles.reportIssueButton,
    ]);

    return (
      <div className={componentStyles}>
        {showReportIssue && (
          <div className={styles.reportIssue}>
            <h1 className={styles.reportIssueText}>
              {isConnecting ?
                intl.formatMessage(messages.reportConnectingIssueText) :
                intl.formatMessage(messages.reportSyncingIssueText)
              }
            </h1>
            <Button
              className={buttonClasses}
              label={intl.formatMessage(messages.reportIssueButtonLabel)}
              onClick={handleReportIssue}
              skin={<SimpleButtonSkin />}
            />
          </div>
        )}
        <div className={styles.logos}>
          <SvgInline svg={currencyLoadingLogo} className={currencyLogoStyles} />
          <SvgInline svg={daedalusLoadingLogo} className={daedalusLogoStyles} />
          <SvgInline svg={apiLoadingLogo} className={apiLogoStyles} />
        </div>
        {hasLoadedCurrentLocale && (
          <div>
            {isConnecting && (
              <div className={styles.connecting}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(connectingMessage)}
                </h1>
              </div>
            )}
            {isSyncing && (
              <div className={styles.syncing}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(messages.syncing)} {syncPercentage.toFixed(2)}%
                </h1>
              </div>
            )}
            {!isSyncing && !isConnecting && isLoadingDataForNextScreen && (
              <div className={styles.syncing}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(loadingDataForNextScreenMessage)}
                </h1>
                <LoadingSpinner />
              </div>
            )}
            {(localTimeDifference > allowedTimeDifference) && (
              <SystemTimeErrorOverlay
                localTimeDifference={localTimeDifference}
                currentLocale={currentLocale}
                onProblemSolutionClick={onProblemSolutionClick}
              />
            )}
          </div>
        )}
      </div>
    );
  }

  connectingTimer = () => {
    this.setState({ connectingTime: this.state.connectingTime + 1 });
  };

  syncingTimer = () => {
    const syncPercentage = this.props.syncPercentage.toFixed(2);

    if (syncPercentage <= this.state.syncPercentage) {
      // syncPercentage not increased, increase syncing time
      this.setState({ syncingTime: this.state.syncingTime + 1 });
    } else {
      // reset syncingTime and set new max percentage
      this.setState({ syncingTime: 0, syncPercentage });
    }
  };
}
