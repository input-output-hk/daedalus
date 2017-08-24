// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import AboutPage from './containers/static/AboutPage';
import { daedalusTheme } from './themes/daedalus';
import translations from './i18n/translations';
import type { StoresMap } from './stores/index';
import ThemeManager from './ThemeManager';

@observer
export default class About extends Component {

  props: {
    stores: StoresMap,
  };

  render() {
    const { stores } = this.props;
    const locale = stores.app.currentLocale;
    const currentTheme = stores.app.currentTheme;
    const theme = require(`./themes/daedalus/${currentTheme}.js`); // eslint-disable-line

    return (
      <div>
        <ThemeManager variables={theme} />
        <ThemeProvider theme={daedalusTheme}>
          <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
            <AboutPage />
          </IntlProvider>
        </ThemeProvider>
      </div>
    );
  }
}
