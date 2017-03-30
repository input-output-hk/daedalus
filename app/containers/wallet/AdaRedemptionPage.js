// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import Wallet from '../../domain/Wallet';
import Request from '../../stores/lib/Request';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import { AdaRedemptionCertificateParseError } from '../../i18n/errors';

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      adaRedemption: PropTypes.shape({
        chooseRedemptionType: PropTypes.func.isRequired,
        redeemAda: PropTypes.func.isRequired,
        setCertificate: PropTypes.func.isRequired,
        setPassPhrase: PropTypes.func.isRequired,
        setRedemptionCode: PropTypes.func.isRequired,
        removeCertificate: PropTypes.func.isRequired,
      }),
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        all: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.instanceOf(Wallet)).isRequired,
      }).isRequired,
      adaRedemption: PropTypes.shape({
        redeemAdaRequest: PropTypes.instanceOf(Request).isRequired,
        certificate: PropTypes.instanceOf(File),
        isCertificateEncrypted: PropTypes.bool.isRequired,
        isValidRedemptionKey: PropTypes.func.isRequired,
        redemptionType: PropTypes.string.isRequired,
        error: PropTypes.instanceOf(Error),
      }).isRequired,
    }).isRequired
  };

  onSubmit = (values: { walletId: string }) => {
    this.props.actions.adaRedemption.redeemAda(values);
  };

  render() {
    const { wallets, adaRedemption } = this.props.stores;
    const {
      redeemAdaRequest, isCertificateEncrypted, isValidRedemptionKey, redemptionType, error
    } = adaRedemption;
    const {
      chooseRedemptionType, setCertificate, setPassPhrase, setRedemptionCode, removeCertificate
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
          onChooseRedemptionType={(choice) => chooseRedemptionType({ redemptionType: choice })}
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
          redemptionType={redemptionType}
        />
      </Layout>
    );
  }
}
