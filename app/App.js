// @flow
import React, { Component } from 'react';
import { MemoryRouter as Router, Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import translations from './i18n/translations';
import { materialUiTheme, daedalusTheme } from './themes/daedalus';
import store from './store';
import Wallet from './containers/wallet/Wallet';

@observer
export default class App extends Component {

  render() {
    const { locale } = store.i18n;
    return (
      <IntlProvider locale={locale} key={locale} messages={translations[locale]}>
        <MuiThemeProvider muiTheme={getMuiTheme(materialUiTheme)}>
          <ThemeProvider theme={daedalusTheme}>
            <Provider store={store}>
              <Router>
                <div>
                  <Match pattern="/" exactly render={() => <Redirect to="/wallet" />} />
                  {/* TODO: Remove redirect after main navigation is implemented */}
                  <Match pattern="/wallet" component={Wallet} />
                </div>
              </Router>
            </Provider>
          </ThemeProvider>
        </MuiThemeProvider>
      </IntlProvider>
    );
  }
}
