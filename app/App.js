// @flow
import React, { Component } from 'react';
import { Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import { action } from 'mobx';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import { intlOptions } from './i18n';
import { daedalusTheme } from './themes/daedalus';
import store from './store';
import Wallet from './containers/wallet/Wallet';

@observer
export default class App extends Component {

  static contextTypes = {
    router: React.PropTypes.object.isRequired
  };

  render() {
    action(() => { store.router = this.context.router; })();
    return (
      <IntlProvider {...intlOptions}>
        <ThemeProvider theme={daedalusTheme}>
          <Provider store={store}>
            <div>
              <Match pattern="/" exactly render={() => <Redirect to="/wallet" />} />
              {/* TODO: Remove redirect after main navigation is implemented */}
              <Match pattern="/wallet" component={Wallet} />
            </div>
          </Provider>
        </ThemeProvider>
      </IntlProvider>
    );
  }
}
