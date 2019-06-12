// @flow
import React, { Component, Fragment } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import DevTools from 'mobx-react-devtools';
import { Router } from 'react-router';
import { IntlProvider } from 'react-intl';
import { Routes } from './Routes';
import { daedalusTheme } from './themes/daedalus';
import { themeOverrides } from './themes/overrides/index.js';
import translations from './i18n/translations';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';
import ThemeManager from './ThemeManager';
import AboutDialog from './containers/static/AboutDialog';
import DaedalusDiagnosticsDialog from './containers/status/DaedalusDiagnosticsDialog';
import GenericNotificationContainer from './containers/notifications/GenericNotificationContainer';

@observer
export default class App extends Component<{
  stores: StoresMap,
  actions: ActionsMap,
  history: Object,
}> {
  componentWillMount() {
    // loads app's global environment variables into AppStore via ipc
    this.props.actions.app.initAppEnvironment.trigger();
  }
  render() {
    const { stores, actions, history } = this.props;
    const { app } = stores;
    const { isAboutDialogOpen, isDaedalusDiagnosticsDialogOpen } = app;
    const locale = stores.profile.currentLocale;
    const mobxDevTools = global.environment.mobxDevTools ? <DevTools /> : null;
    const { currentTheme } = stores.profile;
    const themeVars = require(`./themes/daedalus/${currentTheme}.js`).default;
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
                {isDaedalusDiagnosticsDialogOpen && (
                  <DaedalusDiagnosticsDialog />
                )}
                {isAboutDialogOpen && <AboutDialog />}
                <GenericNotificationContainer />
              </Fragment>
            </IntlProvider>
          </ThemeProvider>
        </Provider>
      </Fragment>
    );
  }
}
