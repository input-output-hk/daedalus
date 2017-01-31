// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import Wallet from '../../domain/Wallet';

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      redeemAda: PropTypes.func.isRequired,
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        all: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.instanceOf(Wallet)).isRequired,
      }).isRequired
    }).isRequired
  };

  onSubmit = (values) => {
    this.props.actions.redeemAda(values);
  };

  render() {
    const wallets = this.props.stores.wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    return (
      <Layout>
        <AdaRedemptionForm
          onSubmit={this.onSubmit}
          wallets={wallets}
          isCertificateUploaded
          isCertificateEncrypted
        />
      </Layout>
    );
  }
}
