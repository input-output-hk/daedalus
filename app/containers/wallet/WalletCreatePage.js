// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';

@inject('actions') @observer
export default class WalletCreatePage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      createPersonalWallet: PropTypes.func.isRequired,
    }),
  };

  createPersonalWallet = (values) => {
    this.props.actions.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  };

  render() {
    return (
      <CenteredLayout>
        <WalletCreateDialog onSubmit={this.createPersonalWallet} />
      </CenteredLayout>
    );
  }
}
