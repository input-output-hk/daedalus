// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';

@observer(['controller'])
export default class WalletSendPage extends Component {

  static propTypes = {
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        sendMoney: PropTypes.func.isRequired,
      })
    }),
  };

  handleWalletSendFormSubmit(values: Object) {
    this.props.controller.wallets.sendMoney(values);
  }

  render() {
    return (
      <WalletSendForm onSubmit={this.handleWalletSendFormSubmit.bind(this)} />
    );
  }

}
