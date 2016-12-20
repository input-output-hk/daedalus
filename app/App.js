// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
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
import LoadingSpinner from './components/widgets/LoadingSpinner';
import environment from './environment';

@observer(['state', 'controller'])
export default class App extends Component {

  static propTypes = {
    state: appStatePropType,
    controller: PropTypes.instanceOf(AppController),
  };

  static contextTypes = {
    router: PropTypes.object.isRequired,
    intl: intlShape.isRequired,
    broadcasts: React.PropTypes.object
  };

  componentDidMount() {
    if (this.context.broadcasts) {
      const subscribe = this.context.broadcasts.location;
      const { state, controller } = this.props;
      const { router, intl } = this.context;
      this.unsubscribeFromLocationBroadcast = subscribe((location) => {
        if (!state.isInitialized) {
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
    const { state, controller } = this.props;
    const { router, intl } = this.context;
    const { isLoggedIn } = state.login;
    controller.setRouter(router);
    controller.setTranslationService(intl);

    if (state.isApplicationLoading) {
      return <div style={{ display: 'flex', alignItems: 'center' }}><LoadingSpinner /></div>;
    }

    let initialPage;

    if (!isLoggedIn) {
      initialPage = (
        <div style={{ height: '100%' }}>
          <Redirect to={'/login'} />
          <Match pattern="/login" component={LoginPage} />
        </div>
      );
    } else {
      const { wallet } = state.activeWallet;
      initialPage = (
        <div style={{ height: '100%' }}>
          <Match pattern="/" exactly render={() => <Redirect to={`/wallet/${wallet.id}/home`} />} />
          <Match pattern="/wallet/:id" component={Wallet} />
          <Match pattern="/settings" component={Settings} />
          <Match pattern="/staking" component={StakingPage} />
        </div>
      );
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
