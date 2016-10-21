// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import walletSendFormValidator from '../../validators/walletSendFormValidator';

@observer(['store'])
export default class WalletSendPage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired
    })
  };

  render() {
    const { wallet } = this.props.store;
    return (
      <WalletWithNavigation wallet={wallet}>
        <WalletSendForm validator={walletSendFormValidator} />
      </WalletWithNavigation>
    );
  }

}
