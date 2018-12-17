// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import adaLogo from '../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import { ellipsis } from '../utils/strings';
import type { InjectedProps } from '../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { getSupportUrl } from '../utils/network';
import NotificationMessage from '../components/widgets/NotificationMessage';
import successIcon from '../assets/images/success-small.inline.svg';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../config/timingConfig';
import { DOWNLOAD_LOGS_SUCCESS_ELLIPSIS } from '../config/formattingConfig';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/categories/360000877653-Daedalus-wallet-mainnet',
    description: 'Link to Open Support page'
  },
  downloadLogsSuccess: {
    id: 'loading.screen.reportIssue.logsDownloadSuccessMessage',
    defaultMessage: '!!!Logs were downloaded',
    description: 'Success message for download logs.',
  },
});

@inject('stores', 'actions') @observer
export default class LoadingPage extends Component<InjectedProps> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { stores } = this.props;
    const {
      cardanoNodeState, isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference, ignoreSystemTimeChecks,
    } = stores.networkStatus;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme, currentLocale } = stores.profile;
    const { id, message } = this.notification;
    return (
      <CenteredLayout>
        <Loading
          currencyIcon={adaLogo}
          apiIcon={cardanoLogo}
          cardanoNodeState={cardanoNodeState}
          isConnected={isConnected}
          isSynced={isSynced}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isCheckingSystemTime={forceCheckTimeDifferenceRequest.isExecuting}
          syncPercentage={syncPercentage}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
          onExternalLinkClick={stores.app.openExternalLink}
          onReportIssueClick={this.handleReportIssueClick}
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
          onDownloadLogs={this.handleDownloadLogs}
        />
        <NotificationMessage
          icon={successIcon}
          show={stores.uiNotifications.isOpen(id)}
          onClose={() => this.props.actions.notifications.closeActiveNotification.trigger({ id })}
          clickToClose
          hasCloseButton
        >
          {message}
        </NotificationMessage>
      </CenteredLayout>
    );
  }

  get notification() {
    const { intl } = this.context;
    return {
      id: 'download-logs-success',
      duration: DOWNLOAD_LOGS_SUCCESS_DURATION,
      message: intl.formatMessage(messages.downloadLogsSuccess),
    };
  }

  handleReportIssueClick = async (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const reportIssueButtonUrl = intl.formatMessage(messages.reportIssueButtonUrl);
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(reportIssueButtonUrl, locale);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    const fileName = generateFileNameWithTimestamp();
    const destination = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.props.actions.profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
    const { id, duration } = this.notification;
    this.props.actions.notifications.open.trigger({ id, duration, });

  };
}
