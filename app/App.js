// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
import { ThemeProvider } from 'react-css-themr';
import { intlShape } from 'react-intl';
import { daedalusTheme } from './themes/daedalus';
import { appStatePropType } from './state/index';
import AppController from './controllers/AppController';
import Wallet from './containers/wallet/Wallet';
import Settings from './containers/settings/Settings';
import WalletCreatePage from './containers/wallet/WalletCreatePage';
import LoadingSpinner from './components/widgets/LoadingSpinner';

@observer(['state', 'controller'])
export default class App extends Component {

  static propTypes = {
    state: appStatePropType,
    controller: PropTypes.instanceOf(AppController),
  };

  static contextTypes = {
    router: PropTypes.object.isRequired,
    intl: intlShape.isRequired,
  };

  render() {
    const { state, controller } = this.props;
    if (state.isApplicationLoading) {
      return <div style={{ display: 'flex', alignItems: 'center' }}><LoadingSpinner /></div>;
    }
    const { activeWallet } = this.props.state;
    const { wallet } = activeWallet;
    controller.initialize(this.context.router, this.context.intl);
    let initialPage;
    if (wallet) {
      initialPage = (
        <div>
          <Match pattern="/" exactly render={() => <Redirect to={`/wallet/${wallet.address}/home`} />} />
          <Match pattern="/wallet/:id" component={Wallet} />
          <Match pattern="/settings" component={Settings} />
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
      <ThemeProvider theme={daedalusTheme}>
        { initialPage }
      </ThemeProvider>
    );
  }
}
