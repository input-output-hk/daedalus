import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletDetailsPage from './WalletDetailsPage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';

@observer(['store'])
export default class Wallet extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired
    }),
    pathname: PropTypes.string.isRequired
  };

  render() {
    const { wallet } = this.props.store;
    const { pathname } = this.props;
    console.log(pathname); // eslint-disable-line
    return (
      <WalletWithNavigation wallet={wallet}>
        <div>
          <Match
            pattern={pathname}
            exactly
            render={() => (
              <Redirect to={`${pathname}/details`} />
            )}
          />
          <Match pattern={`${pathname}/details`} component={WalletDetailsPage} />
          <Match pattern={`${pathname}/send`} component={WalletSendPage} />
          <Match pattern={`${pathname}/receive`} component={WalletReceivePage} />
        </div>
      </WalletWithNavigation>
    );
  }

}
