// @flow
import React, { Component } from 'react';
import { Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import { intlOptions } from './i18n';
import { daedalusTheme } from './themes/daedalus';
import state from './state';
import Wallet from './containers/wallet/Wallet';

@observer
export default class App extends Component {

  static contextTypes = {
    router: React.PropTypes.object.isRequired
  };

  render() {
    state.uiStore.setRouter(this.context.router);
    return (
      <IntlProvider {...intlOptions}>
        <ThemeProvider theme={daedalusTheme}>
          <Provider {...state.uiStore} walletsForSidebar={state.uiStore.walletsForSidebar}>
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
