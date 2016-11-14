// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';

@observer
export default class WalletSendPage extends Component {

  static propTypes = {
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        sendMoney: PropTypes.func.isRequired,
      })
    }),
  }

  render() {
    return (
      <WalletSendForm onSubmit={this.props.controller.wallets.sendMoney} />
    );
  }

}
