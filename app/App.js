// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { inject, observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import { intlShape } from 'react-intl';
import DevTools from 'mobx-react-devtools';
import { daedalusTheme } from './themes/daedalus';
import Wallet from './containers/wallet/Wallet';
import Settings from './containers/settings/Settings';
import StakingPage from './containers/staking/StakingPage';
import LoginPage from './containers/login/LoginPage';
import WalletCreatePage from './containers/wallet/WalletCreatePage';
import LoadingSpinner from './components/widgets/LoadingSpinner';
import environment from './environment';
import { storesPropType } from './propTypes';

@inject('stores') @observer
export default class App extends Component {

  static propTypes = {
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
      const { app } = this.props.stores;
      this.unsubscribeFromLocationBroadcast = subscribe((location) => {
        app.updateLocation(location);
      });
    }
  }

  componentWillUnmount() {
    if (this.unsubscribeFromLocationBroadcast) this.unsubscribeFromLocationBroadcast();
  }

  unsubscribeFromLocationBroadcast: () => {};

  render() {
    const { stores } = this.props;
    const { router, intl } = this.context;
    const { user, wallets } = stores;

    stores.app.initialize(router, intl);

    const loadingSpinner = (
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <Redirect to={'/'} />
        <LoadingSpinner />
      </div>
    );

    if (!stores.app.isInitialized) return loadingSpinner;

    let initialPage;

    if (!user.isLoggedIn) {
      initialPage = (
        <div style={{ height: '100%' }}>
          <Redirect to={'/login'} />
          <Match pattern="/login" component={LoginPage} />
        </div>
      );
    } else if (wallets.walletsRequest.isExecutingFirstTime) {
      return loadingSpinner;
    } else if (wallets.all.length > 0) {
      const wallet = wallets.active;
      initialPage = (
        <div style={{ height: '100%' }}>
          <Match pattern="/" exactly render={() => <Redirect to={`${wallets.BASE_ROUTE}/${wallet.id}/home`} />} />
          <Match pattern={`${wallets.BASE_ROUTE}/:id`} component={Wallet} />
          <Match pattern="/settings" component={Settings} />
          <Match pattern="/staking" component={StakingPage} />
        </div>
      );
    } else {
      initialPage = <WalletCreatePage />;
    }

    const mobxDevTools = environment.MOBX_DEV_TOOLS ? <DevTools /> : null;
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
