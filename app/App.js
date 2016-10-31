// @flow
import React, { Component } from 'react';
import { MemoryRouter as Router, Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import translations from './i18n/translations';
import { daedalusTheme } from './themes/daedalus';
import store from './store';
import Wallet from './containers/wallet/Wallet';

const { locale } = store.i18n;

const intlOptions = {
  locale,
  key: locale,
  messages: translations[locale]
};

export const { intl } = new IntlProvider(intlOptions, {}).getChildContext();

@observer
export default class App extends Component {
  render() {
    return (
      <IntlProvider {...intlOptions}>
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
      </IntlProvider>
    );
  }
}
