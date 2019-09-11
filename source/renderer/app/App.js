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
import ThemeManager from './ThemeManager';
import AboutDialog from './containers/static/AboutDialog';
import DaedalusDiagnosticsDialog from './containers/status/DaedalusDiagnosticsDialog';
import BlockConsolidationStatusDialog from './containers/status/BlockConsolidationStatusDialog';
import GenericNotificationContainer from './containers/notifications/GenericNotificationContainer';
import AutomaticUpdateNotificationDialog from './containers/notifications/AutomaticUpdateNotificationDialog';
import { DIALOGS } from '../../common/ipc/constants';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';

@observer
export default class App extends Component<{
  stores: StoresMap,
  actions: ActionsMap,
  history: Object,
}> {
  componentWillMount() {
    // Loads app's global environment variables into AppStore via ipc
    this.props.actions.app.initAppEnvironment.trigger();
  }
  render() {
    const { stores, actions, history } = this.props;
    const { app, nodeUpdate } = stores;
    const { showNextUpdate } = nodeUpdate;
    const { isActiveDialog } = app;
    const locale = stores.profile.currentLocale;
    const mobxDevTools = global.environment.mobxDevTools ? <DevTools /> : null;
    const { currentTheme } = stores.profile;
    const { isIncentivizedTestnet } = stores.networkStatus;
    console.log(isIncentivizedTestnet);
    const themeVars = isIncentivizedTestnet
      ? require(`./themes/daedalus/yellow.js`)
      : require(`./themes/daedalus/${currentTheme}.js`).default;
    const { ABOUT, BLOCK_CONSOLIDATION, DAEDALUS_DIAGNOSTICS } = DIALOGS;

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
                {showNextUpdate ? (
                  <AutomaticUpdateNotificationDialog />
                ) : (
                  [
                    isActiveDialog(ABOUT) && <AboutDialog />,
                    isActiveDialog(BLOCK_CONSOLIDATION) && (
                      <BlockConsolidationStatusDialog />
                    ),
                    isActiveDialog(DAEDALUS_DIAGNOSTICS) && (
                      <DaedalusDiagnosticsDialog />
                    ),
                    <GenericNotificationContainer key="genericNotification" />,
                  ]
                )}
              </Fragment>
            </IntlProvider>
          </ThemeProvider>
        </Provider>
      </Fragment>
    );
  }
}
