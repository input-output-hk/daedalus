// @flow
import React, { Component } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import { Router } from 'react-router';
import { IntlProvider } from 'react-intl';
import Routes from './Routes';
import AboutPage from './containers/static/AboutPage';
import { daedalusTheme } from './themes/daedalus';
import translations from './i18n/translations';
import type { StoresMap } from './stores/index';
import type { ActionsMap } from './actions/index';
import ThemeManager from './ThemeManager';

@observer
export default class SystemMenuProvider extends Component {

  props: {
    stores: StoresMap,
    actions: ActionsMap,
    history: Object,
    route: String,
  };

  render() {
    const { stores, actions, history, route } = this.props;
    const locale = stores.app.currentLocale;
    const currentTheme = stores.app.currentTheme;
    const theme = require(`./themes/daedalus/${currentTheme}.js`); // eslint-disable-line

    let page;
    if (route === 'about') {
        page = <AboutPage />;
    }

    return (
      <div>
        <ThemeManager variables={theme} />
        <Provider stores={stores} actions={actions}>
          <ThemeProvider theme={daedalusTheme}>
            <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
              {page}
            </IntlProvider>
          </ThemeProvider>
        </Provider>
      </div>
    );
  }
}
