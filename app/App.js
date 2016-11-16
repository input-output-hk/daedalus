// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { Provider, observer } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-css-themr';
import { daedalusTheme } from './themes/daedalus';
import { appStatePropType } from './state/index';
import AppController from './controllers/AppController';
import Wallet from './containers/wallet/Wallet';
import translations from './i18n/translations';
import WalletCreatePage from './containers/wallet/WalletCreatePage';

@observer
export default class App extends Component {

  static propTypes = {
    state: appStatePropType,
    controller: PropTypes.instanceOf(AppController),
  };

  static contextTypes = {
    router: PropTypes.object.isRequired,
  };

  render() {
    const { state, controller } = this.props;
    if (state.activeWallet.isLoading) {
      return <div>Loading...</div>;
    }
    const { activeWallet } = this.props.state;
    const { wallet } = activeWallet;
    controller.setAppRouter(this.context.router);
    const locale = state.i18n.locale;
    let initialPage;
    if (wallet) {
      initialPage = (
        <div>
          <Match pattern="/" exactly render={() => <Redirect to={`/wallet/${wallet.address}/home`} />} />
          <Match pattern="/wallet/:id" component={Wallet} />
        </div>
      );
    } else {
      initialPage = (
        <div style={{ height: '100%' }}>
          <Match pattern="/" render={() => <Redirect to="/create-first-wallet" />} />
          <Match pattern="/create-first-wallet" component={WalletCreatePage} />
        </div>
      );
    }
    return (
      <IntlProvider {...{ locale, key: locale, messages: translations[locale] }}>
        <ThemeProvider theme={daedalusTheme}>
          <Provider state={state} controller={controller}>
            { initialPage }
          </Provider>
        </ThemeProvider>
      </IntlProvider>
    );
  }
}
