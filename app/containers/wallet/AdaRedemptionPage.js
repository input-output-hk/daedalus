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
        redeemPaperVendedAda: PropTypes.func.isRequired,
        setCertificate: PropTypes.func.isRequired,
        setPassPhrase: PropTypes.func.isRequired,
        setEmail: PropTypes.func.isRequired,
        setAdaPasscode: PropTypes.func.isRequired,
        setAdaAmount: PropTypes.func.isRequired,
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
        redeemPaperVendedAdaRequest: PropTypes.instanceOf(Request).isRequired,
        certificate: PropTypes.instanceOf(File),
        isCertificateEncrypted: PropTypes.bool.isRequired,
        isValidRedemptionKey: PropTypes.func.isRequired,
        isValidRedemptionMnemonic: PropTypes.func.isRequired,
        isValidPostVendRedeemCode: PropTypes.func.isRequired,
        redemptionType: PropTypes.string.isRequired,
        error: PropTypes.instanceOf(Error),
      }).isRequired,
    }).isRequired
  };

  onSubmit = (values: { walletId: string }) => {
    this.props.actions.adaRedemption.redeemAda(values);
  };

  onSubmitPaperVended = (values: { walletId: string, shieldedRedemptionKey: string }) => {
    this.props.actions.adaRedemption.redeemPaperVendedAda(values);
  };

  render() {
    const { wallets, adaRedemption } = this.props.stores;
    const {
      redeemAdaRequest, redeemPaperVendedAdaRequest, isCertificateEncrypted, isValidRedemptionKey,
      redemptionType, isValidRedemptionMnemonic, isValidPostVendRedeemCode, error
    } = adaRedemption;
    const {
      chooseRedemptionType, setCertificate, setPassPhrase, setRedemptionCode, removeCertificate,
      setEmail, setAdaPasscode, setAdaAmount
    } = this.props.actions.adaRedemption;

    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    if (selectableWallets.length === 0) return <Layout><LoadingSpinner /></Layout>;
    const request = redemptionType === 'paperVended' ? redeemPaperVendedAdaRequest : redeemAdaRequest;
    const isCertificateSelected = adaRedemption.certificate !== null;
    const showInputsForDecryptingForceVendedCertificate = isCertificateSelected && isCertificateEncrypted
      && redemptionType === 'forceVended';
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted && redemptionType === 'regular' ||
      redemptionType === 'paperVended';
    return (
      <Layout>
        <AdaRedemptionForm
          onCertificateSelected={(certificate) => setCertificate({ certificate })}
          onPassPhraseChanged={(passPhrase) => setPassPhrase({ passPhrase })}
          onEmailChanged={(email) => setEmail({ email })}
          onAdaAmountChanged={(adaAmount) => setAdaAmount({ adaAmount })}
          onAdaPasscodeChanged={(adaPasscode) => setAdaPasscode({ adaPasscode })}
          onRedemptionCodeChanged={(redemptionCode) => setRedemptionCode({ redemptionCode })}
          onChooseRedemptionType={(choice) => chooseRedemptionType({ redemptionType: choice })}
          redemptionCode={adaRedemption.redemptionCode}
          wallets={selectableWallets}
          isCertificateSelected={isCertificateSelected}
          isCertificateEncrypted={isCertificateEncrypted}
          isCertificateInvalid={error && error instanceof AdaRedemptionCertificateParseError}
          isSubmitting={request.isExecuting}
          error={adaRedemption.error}
          onSubmit={redemptionType === 'paperVended' ? this.onSubmitPaperVended : this.onSubmit}
          onRemoveCertificate={removeCertificate}
          mnemonicValidator={isValidRedemptionMnemonic}
          redemptionCodeValidator={isValidRedemptionKey}
          postVendRedemptionCodeValidator={isValidPostVendRedeemCode}
          redemptionType={redemptionType}
          showInputsForDecryptingForceVendedCertificate={showInputsForDecryptingForceVendedCertificate}
          showPassPhraseWidget={showPassPhraseWidget}
        />
      </Layout>
    );
  }
}
