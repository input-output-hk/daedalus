// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import { AdaRedemptionCertificateParseError } from '../../i18n/errors';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component {

  props: InjectedProps;

  onSubmit = (values: { walletId: string }) => {
    this.props.actions.adaRedemption.redeemAda(values);
  };

  render() {
    const { wallets, adaRedemption } = this.props.stores;
    const { redeemAdaRequest, isCertificateEncrypted, isValidRedemptionKey, error } = adaRedemption;
    const {
      setCertificate, setPassPhrase, setRedemptionCode, removeCertificate
    } = this.props.actions.adaRedemption;
    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    if (selectableWallets.length === 0) return <Layout><LoadingSpinner /></Layout>;

    return (
      <Layout>
        <AdaRedemptionForm
          onCertificateSelected={(certificate) => setCertificate({ certificate })}
          onPassPhraseChanged={(passPhrase) => setPassPhrase({ passPhrase })}
          onRedemptionCodeChanged={(redemptionCode) => setRedemptionCode({ redemptionCode })}
          redemptionCode={adaRedemption.redemptionCode}
          wallets={selectableWallets}
          isCertificateSelected={adaRedemption.certificate !== null}
          isCertificateEncrypted={isCertificateEncrypted}
          isCertificateInvalid={error && error instanceof AdaRedemptionCertificateParseError}
          isSubmitting={redeemAdaRequest.isExecuting}
          error={adaRedemption.error}
          onSubmit={this.onSubmit}
          onRemoveCertificate={removeCertificate}
          redemptionCodeValidator={isValidRedemptionKey}
        />
      </Layout>
    );
  }
}
