// @flow
import React, { Component } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import DevTools from 'mobx-react-devtools';
import { Router } from 'react-router';
import { IntlProvider } from 'react-intl';
import Routes from './Routes';
import { daedalusTheme } from './themes/daedalus';
import environment from './environment';
import translations from './i18n/translations';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';

@observer
export default class App extends Component {

  props: {
    stores: StoresMap,
    actions: ActionsMap,
    history: Object,
  };

  render() {
    const { stores, actions, history } = this.props;
    const locale = stores.app.currentLocale;
    const mobxDevTools = environment.MOBX_DEV_TOOLS ? <DevTools /> : null;

    return (
      <Provider stores={stores} actions={actions}>
        <ThemeProvider theme={daedalusTheme}>
          <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
            <div style={{ height: '100%' }}>
              <Router history={history} routes={Routes} />
              {mobxDevTools}
            </div>
          </IntlProvider>
        </ThemeProvider>
      </Provider>
    );
  }
}
