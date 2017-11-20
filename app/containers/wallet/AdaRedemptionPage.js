// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import { AdaRedemptionCertificateParseError } from '../../i18n/errors';
import type { InjectedProps } from '../../types/injectedPropsType';
import validWords from '../../../lib/valid-words.en';
import environment from '../../environment';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: { walletId: string, walletPassword: ?string }) => {
    this.props.actions.ada.adaRedemption.redeemAda.trigger(values);
  };

  onSubmitPaperVended = (values: {
    walletId: string,
    shieldedRedemptionKey: string,
    walletPassword: ?string,
  }) => {
    this.props.actions.ada.adaRedemption.redeemPaperVendedAda.trigger(values);
  };

  render() {
    const { wallets, adaRedemption } = this.props.stores.ada;
    const {
      redeemAdaRequest, redeemPaperVendedAdaRequest, isCertificateEncrypted, isValidRedemptionKey,
      redemptionType, isValidRedemptionMnemonic, isValidPaperVendRedemptionKey,
      isRedemptionDisclaimerAccepted, error
    } = adaRedemption;
    const {
      chooseRedemptionType, setCertificate, setPassPhrase, setRedemptionCode, removeCertificate,
      setEmail, setAdaPasscode, setAdaAmount, acceptRedemptionDisclaimer
    } = this.props.actions.ada.adaRedemption;
    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    if (selectableWallets.length === 0) return <Layout><LoadingSpinner /></Layout>;
    const request = redemptionType === 'paperVended' ? redeemPaperVendedAdaRequest : redeemAdaRequest;
    const isCertificateSelected = adaRedemption.certificate !== null;
    const showInputsForDecryptingForceVendedCertificate = isCertificateSelected &&
      isCertificateEncrypted && redemptionType === 'forceVended';
    const showPassPhraseWidget = isCertificateSelected && isCertificateEncrypted &&
      redemptionType === 'regular' || redemptionType === 'paperVended';
    return (
      <Layout>
        <AdaRedemptionForm
          onCertificateSelected={(certificate) => setCertificate.trigger({ certificate })}
          onPassPhraseChanged={(passPhrase) => setPassPhrase.trigger({ passPhrase })}
          onRedemptionCodeChanged={(redemptionCode) => {
            setRedemptionCode.trigger({ redemptionCode });
          }}
          onEmailChanged={(email) => setEmail.trigger({ email })}
          onAdaAmountChanged={(adaAmount) => setAdaAmount.trigger({ adaAmount })}
          onAdaPasscodeChanged={(adaPasscode) => setAdaPasscode.trigger({ adaPasscode })}
          onChooseRedemptionType={(choice) => {
            chooseRedemptionType.trigger({ redemptionType: choice });
          }}
          redemptionCode={adaRedemption.redemptionCode}
          wallets={selectableWallets}
          suggestedMnemonics={validWords}
          isCertificateSelected={isCertificateSelected}
          isCertificateEncrypted={isCertificateEncrypted}
          isCertificateInvalid={error instanceof AdaRedemptionCertificateParseError}
          isSubmitting={request.isExecuting}
          error={adaRedemption.error}
          onRemoveCertificate={removeCertificate.trigger}
          onSubmit={redemptionType === 'paperVended' ? this.onSubmitPaperVended : this.onSubmit}
          mnemonicValidator={isValidRedemptionMnemonic}
          redemptionCodeValidator={isValidRedemptionKey}
          postVendRedemptionCodeValidator={isValidPaperVendRedemptionKey}
          redemptionType={redemptionType}
          showInputsForDecryptingForceVendedCertificate={
            showInputsForDecryptingForceVendedCertificate
          }
          showPassPhraseWidget={showPassPhraseWidget}
          isRedemptionDisclaimerAccepted={environment.isMainnet() || isRedemptionDisclaimerAccepted}
          onAcceptRedemptionDisclaimer={() => acceptRedemptionDisclaimer.trigger()}
          getSelectedWallet={walletId => wallets.getWalletById(walletId)}
        />
      </Layout>
    );
  }
}
