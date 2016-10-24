// @flow
import React, { Component } from 'react';
import { MemoryRouter as Router, Match } from 'react-router';
import { Provider, observer } from 'mobx-react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import { IntlProvider } from 'react-intl';
import translations from './i18n/translations';
import daedalusTheme from './themes/daedalus';
import store from './store';
import Wallet from './containers/wallet/Wallet';

@observer
export default class App extends Component {

  render() {
    const { locale } = store.i18n;
    return (
      <IntlProvider locale={locale} key={locale} messages={translations[locale]}>
        <MuiThemeProvider muiTheme={getMuiTheme(daedalusTheme)}>
          <Provider store={store}>
            <Router>
              <div>
                <Match exactly pattern="/" component={Wallet} />
                <Match pattern="/wallet" component={Wallet} />
              </div>
            </Router>
          </Provider>
        </MuiThemeProvider>
      </IntlProvider>
    );
  }
}
