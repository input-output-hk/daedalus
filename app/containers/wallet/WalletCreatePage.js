// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletCreateForm from '../../components/wallet/WalletCreateForm';

@observer(['controller'])
export default class WalletCreatePage extends Component {

  static propTypes = {
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        createPersonalWallet: PropTypes.func.isRequired,
      })
    }),
  };

  handleFormSubmit(values: Object) {
    this.props.controller.wallets.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  }

  render() {
    return (
      <WalletCreateForm onSubmit={this.handleFormSubmit.bind(this)} />
    );
  }

}
