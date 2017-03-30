// @flow
import React, { Component, PropTypes } from 'react';
import { Provider, observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import DevTools from 'mobx-react-devtools';
import { Router } from 'react-router';
import { IntlProvider } from 'react-intl';
import Routes from './Routes';
import { daedalusTheme } from './themes/daedalus';
import environment from './environment';
import translations from './i18n/translations';

@observer
export default class App extends Component {

  static propTypes = {
    stores: PropTypes.shape({}).isRequired,
    actions: PropTypes.shape({}).isRequired,
    history: PropTypes.shape({}).isRequired,
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
