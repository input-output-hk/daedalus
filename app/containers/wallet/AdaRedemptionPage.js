// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import Wallet from '../../domain/Wallet';
import Request from '../../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      redeemAda: PropTypes.func.isRequired,
      setRedemptionCertificate: PropTypes.func.isRequired,
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        all: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.instanceOf(Wallet)).isRequired,
      }).isRequired,
      adaRedemption: PropTypes.shape({
        redeemAdaRequest: PropTypes.instanceOf(Request).isRequired,
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
    const { redeemAdaRequest, certificate, isCertificateEncrypted, error, redemptionCode } = adaRedemption;
    const { setRedemptionCertificate, setRedemptionPassPhrase, setRedemptionCode } = this.props.actions;
    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    return (
      <Layout>
        <AdaRedemptionForm
          onCertificateSelected={(certificate) => setRedemptionCertificate({ certificate })}
          onPassPhraseChanged={(passPhrase) => setRedemptionPassPhrase({ passPhrase })}
          onRedemptionCodeChanged={(redemptionCode) => setRedemptionCode({ redemptionCode })}
          redemptionCode={redemptionCode}
          wallets={selectableWallets}
          isCertificateSelected={certificate !== null}
          isCertificateEncrypted={isCertificateEncrypted}
          isSubmitting={redeemAdaRequest.isExecuting}
          error={error}
          onSubmit={this.onSubmit}
        />
      </Layout>
    );
  }
}
