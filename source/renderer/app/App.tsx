import React, { Component, Fragment } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import DevTools from 'mobx-react-devtools';
import { Router } from 'react-router-dom';
import { IntlProvider } from 'react-intl';
import { ThemeProvider as EmotionThemeProvider } from '@emotion/react';
import { Routes } from './Routes';
import { daedalusTheme } from './themes/daedalus';
import { themeOverrides } from './themes/overrides';
import translations from './i18n/translations';
import ThemeManager from './ThemeManager';
import AboutDialog from './containers/static/AboutDialog';
import DaedalusDiagnosticsDialog from './containers/status/DaedalusDiagnosticsDialog';
import NotificationsContainer from './containers/notifications/NotificationsContainer';
import NewsOverlayContainer from './containers/news/NewsOverlayContainer';
import { DIALOGS } from '../../common/ipc/constants';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';
import NewsFeedContainer from './containers/news/NewsFeedContainer';

@observer
class App extends Component<{
  stores: StoresMap;
  actions: ActionsMap;
  history: Record<string, any>;
}> {
  componentDidMount() {
    // Loads app's global environment variables into AppStore via ipc
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.props.actions.app.initAppEnvironment.trigger();
  }

  render() {
    const { stores, actions, history } = this.props;
    const { app, networkStatus } = stores;
    const { isActiveDialog, isSetupPage } = app;
    const { isNodeStopping, isNodeStopped } = networkStatus;
    const locale = stores.profile.currentLocale;
    const mobxDevTools = global.environment.mobxDevTools ? <DevTools /> : null;
    const { currentTheme } = stores.profile;

    const themeVars = require(`./themes/daedalus/${currentTheme}.ts`).default;

    const { ABOUT, DAEDALUS_DIAGNOSTICS } = DIALOGS;
    const canShowNews =
      !isSetupPage && // Active page is not "Language Selection" or "Terms of Use"
      !isNodeStopping && // Daedalus is not shutting down
      !isNodeStopped;

    return (
      <EmotionThemeProvider theme={{}}>
        <Fragment>
          {/* @ts-ignore */}
          <ThemeManager variables={themeVars} />
          <Provider stores={stores} actions={actions}>
            <ThemeProvider
              theme={daedalusTheme}
              skins={SimpleSkins}
              variables={SimpleDefaults}
              themeOverrides={themeOverrides}
            >
              <IntlProvider
                {...{ locale, key: locale, messages: translations[locale] }}
              >
                <Fragment>
                  <Router history={history}>
                    <Routes />
                  </Router>
                  {mobxDevTools}
                  {[
                    // @ts-ignore Argument of type 'string' is not assignable to parameter of type 'ApplicationDialog'.ts(2345)
                    isActiveDialog(ABOUT) && <AboutDialog key="aboutDialog" />,
                    // @ts-ignore Argument of type 'string' is not assignable to parameter of type 'ApplicationDialog'.ts(2345)
                    isActiveDialog(DAEDALUS_DIAGNOSTICS) && (
                      <DaedalusDiagnosticsDialog key="daedalusDiagnosticsDialog" />
                    ),
                    <NotificationsContainer key="notificationsContainer" />,
                  ]}
                  {canShowNews && [
                    <NewsFeedContainer key="newsFeedList" />,
                    <NewsOverlayContainer key="newsFeedOverlay" />,
                  ]}
                </Fragment>
              </IntlProvider>
            </ThemeProvider>
          </Provider>
        </Fragment>
      </EmotionThemeProvider>
    );
  }
}

export default App;
