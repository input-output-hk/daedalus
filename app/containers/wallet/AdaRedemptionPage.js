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
      updateRedemptionCertificate: PropTypes.func.isRequired,
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        all: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.instanceOf(Wallet)).isRequired,
      }).isRequired,
      adaRedemption: PropTypes.shape({
        isProcessing: PropTypes.bool.isRequired,
        certificate: PropTypes.instanceOf(File),
        isCertificateEncrypted: PropTypes.bool.isRequired,
        error: PropTypes.instanceOf(Error),
      }).isRequired,
    }).isRequired
  };

  onSubmit = (values) => {
    this.props.actions.redeemAda(values);
  };

  render() {
    const { wallets, adaRedemption } = this.props.stores;
    const { isProcessing, certificate, isCertificateEncrypted} = adaRedemption;
    const { updateRedemptionCertificate } = this.props.actions;
    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    return (
      <Layout>
        <AdaRedemptionForm
          onCertificateSelected={(certificate) => updateRedemptionCertificate({ certificate })}
          onSubmit={this.onSubmit}
          wallets={selectableWallets}
          isCertificateSelected={certificate !== null}
          isCertificateEncrypted={isCertificateEncrypted}
          isSubmitting={isProcessing}
        />
      </Layout>
    );
  }
}
