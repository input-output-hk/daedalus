// @flow
import React, { Component, Fragment } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import DevTools from 'mobx-react-devtools';
import { Router } from 'react-router';
import { defineMessages, IntlProvider, intlShape } from 'react-intl';
import { Routes } from './Routes';
import { daedalusTheme } from './themes/daedalus';
import { themeOverrides } from './themes/overrides/index.js';
import translations from './i18n/translations';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';
import ThemeManager from './ThemeManager';
import AboutDialog from './containers/static/AboutDialog';
import NetworkStatusDialog from './containers/status/NetworkStatusDialog';
import GenericNotificationContainer from './containers/notifications/GenericNotificationContainer';
import GenericNotification from './components/notifications/GenericNotification';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from './config/timingConfig';
import { i18nContext } from './utils/i18nContext';

export const messages = defineMessages({
  downloadLogsSuccess: {
    id: 'loading.screen.reportIssue.downloadLogsSuccessMessage',
    defaultMessage: '!!!Logs successfully downloaded',
    description: 'Success message for download logs.',
  },
  downloadLogsProgress: {
    id: 'loading.screen.reportIssue.downloadLogsProgressMessage',
    defaultMessage: '!!!Preparing logs for download',
    description: 'Progress message for download logs.',
  },
});

const DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID =
  'loading-page-download-logs-progress';
const DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID =
  'loading-page-download-logs-success';

@observer
export default class App extends Component<{
  stores: StoresMap,
  actions: ActionsMap,
  history: Object,
}> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillMount() {
    // loads app's global environment variables into AppStore via ipc
    this.props.actions.app.initAppEnvironment.trigger();
  }
  render() {
    const { stores, actions, history } = this.props;
    const { app } = stores;
    const { isAboutDialogOpen, isNetworkStatusDialogOpen } = app;
    const locale = stores.profile.currentLocale;
    const intl = i18nContext(locale);
    const mobxDevTools = global.environment.mobxDevTools ? <DevTools /> : null;
    const { currentTheme } = stores.profile;
    const themeVars = require(`./themes/daedalus/${currentTheme}.js`).default;

    // Notification component z-index
    const notificationOrder = 99999;

    return (
      <Fragment>
        <ThemeManager variables={themeVars} />
        <Provider stores={stores} actions={actions}>
          <ThemeProvider theme={daedalusTheme} themeOverrides={themeOverrides}>
            <IntlProvider
              {...{ locale, key: locale, messages: translations[locale] }}
            >
              <Fragment>
                <Router history={history} routes={Routes} />
                {mobxDevTools}
                {isNetworkStatusDialogOpen && <NetworkStatusDialog />}
                {isAboutDialogOpen && <AboutDialog />}
                {
                  <GenericNotificationContainer>
                    <GenericNotification
                      id={DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID}
                      show={stores.uiNotifications.isOpen(
                        DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID
                      )}
                      actionToListenAndOpen={actions.profile.downloadLogs}
                      actionToListenAndClose={
                        actions.profile.downloadLogsSuccess
                      }
                      icon="spinner"
                      hasEllipsis
                      hasCloseButton
                      order={notificationOrder}
                    >
                      {intl.formatMessage(messages.downloadLogsProgress)}
                    </GenericNotification>
                    <GenericNotification
                      id={DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID}
                      duration={DOWNLOAD_LOGS_SUCCESS_DURATION}
                      show={stores.uiNotifications.isOpen(
                        DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID
                      )}
                      actionToListenAndOpen={
                        actions.profile.downloadLogsSuccess
                      }
                      actionToListenAndClose={actions.profile.downloadLogs}
                      icon="success"
                      hasCloseButton
                      order={notificationOrder}
                    >
                      {intl.formatMessage(messages.downloadLogsSuccess)}
                    </GenericNotification>
                  </GenericNotificationContainer>
                }
              </Fragment>
            </IntlProvider>
          </ThemeProvider>
        </Provider>
      </Fragment>
    );
  }
}
