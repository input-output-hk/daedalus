// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { inject, observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import { intlShape } from 'react-intl';
import DevTools from 'mobx-react-devtools';
import { daedalusTheme } from './themes/daedalus';
import { appStatePropType } from './state/index';
import AppController from './controllers/AppController';
import Wallet from './containers/wallet/Wallet';
import Settings from './containers/settings/Settings';
import StakingPage from './containers/staking/StakingPage';
import LoginPage from './containers/login/LoginPage';
import WalletCreatePage from './containers/wallet/WalletCreatePage';
import LoadingSpinner from './components/widgets/LoadingSpinner';
import environment from './environment';
import { storesPropType } from './propTypes';

@inject('state', 'controller', 'stores') @observer
export default class App extends Component {

  static propTypes = {
    state: appStatePropType,
    controller: PropTypes.instanceOf(AppController),
    stores: storesPropType,
  };

  static contextTypes = {
    router: PropTypes.object.isRequired,
    intl: intlShape.isRequired,
    broadcasts: React.PropTypes.object
  };

  componentDidMount() {
    if (this.context.broadcasts) {
      const subscribe = this.context.broadcasts.location;
      const { controller, stores } = this.props;
      const { router, intl } = this.context;
      this.unsubscribeFromLocationBroadcast = subscribe((location) => {
        if (!stores.app.isInitialized) {
          controller.initialize(router, location, intl);
        } else {
          controller.updateLocation(location);
        }
      });
    }
  }

  componentWillUnmount() {
    if (this.unsubscribeFromLocationBroadcast) this.unsubscribeFromLocationBroadcast();
  }

  unsubscribeFromLocationBroadcast: () => {};

  render() {
    const { state, controller, stores } = this.props;
    const { router, intl } = this.context;
    const { user } = stores;
    controller.setRouter(router);
    controller.setTranslationService(intl);

    if (!stores.app.isInitialized) {
      return <div style={{ display: 'flex', alignItems: 'center' }}><LoadingSpinner /></div>;
    }

    let initialPage;

    if (!user.isLoggedIn) {
      initialPage = (
        <div style={{ height: '100%' }}>
          <Redirect to={'/login'} />
          <Match pattern="/login" component={LoginPage} />
        </div>
      );
    } else if (user.active.wallets.length > 0) {
      const { wallet } = state.activeWallet;
      initialPage = (
        <div style={{ height: '100%' }}>
          <Match pattern="/" exactly render={() => <Redirect to={`/wallet/${wallet.id}/home`} />} />
          <Match pattern="/wallet/:id" component={Wallet} />
          <Match pattern="/settings" component={Settings} />
          <Match pattern="/staking" component={StakingPage} />
        </div>
      );
    } else {
      initialPage = <WalletCreatePage />;
    }

    const mobxDevTools = environment.isDev() ? <DevTools /> : null;
    return (
      <ThemeProvider theme={daedalusTheme}>
        <div style={{ height: '100%' }}>
          {initialPage}
          {mobxDevTools}
        </div>
      </ThemeProvider>
    );
  }
}
