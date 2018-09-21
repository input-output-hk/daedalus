// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import AdaRedemptionForm from '../../components/wallet/ada-redemption/AdaRedemptionForm';
import AdaRedemptionNoWallets from '../../components/wallet/ada-redemption/AdaRedemptionNoWallets';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import { AdaRedemptionCertificateParseError } from '../../i18n/errors';
import type { InjectedProps } from '../../types/injectedPropsType';
import validWords from '../../../../common/valid-words.en';
import environment from '../../../../common/environment';
import { ADA_REDEMPTION_TYPES } from '../../types/redemptionTypes';
import { ROUTES } from '../../routes-config';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class AdaRedemptionPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onSubmit = (values: { walletId: string, spendingPassword: ?string }) => {
    this.props.actions.ada.adaRedemption.redeemAda.trigger(values);
  };

  onSubmitPaperVended = (values: {
    walletId: string,
    shieldedRedemptionKey: string,
    spendingPassword: ?string,
  }) => {
    this.props.actions.ada.adaRedemption.redeemPaperVendedAda.trigger(values);
  };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
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
      setEmail, setAdaPasscode, setAdaAmount, setDecryptionKey, acceptRedemptionDisclaimer
    } = this.props.actions.ada.adaRedemption;
    const selectableWallets = wallets.all.map((w) => ({
      value: w.id, label: w.name
    }));

    if (!wallets.all.length) {
      return (
        <Layout>
          <AdaRedemptionNoWallets
            onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
          />
        </Layout>
      );
    }

    if (selectableWallets.length === 0) return <Layout><LoadingSpinner /></Layout>;

    const request = (redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED ?
      redeemPaperVendedAdaRequest : redeemAdaRequest
    );
    const isCertificateSelected = adaRedemption.certificate !== null;

    const showInputsForDecryptingForceVendedCertificate = (
      isCertificateSelected && isCertificateEncrypted &&
      redemptionType === ADA_REDEMPTION_TYPES.FORCE_VENDED
    );
    const showInputForDecryptionKey = (
      isCertificateSelected && isCertificateEncrypted &&
      redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_FORCE_VENDED
    );
    const showPassPhraseWidget = redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED || (
      isCertificateSelected && isCertificateEncrypted && (
        redemptionType === ADA_REDEMPTION_TYPES.REGULAR ||
        redemptionType === ADA_REDEMPTION_TYPES.RECOVERY_REGULAR
      )
    );

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
          onDecryptionKeyChanged={(decryptionKey) => setDecryptionKey.trigger({ decryptionKey })}
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
          onSubmit={(redemptionType === ADA_REDEMPTION_TYPES.PAPER_VENDED ?
            this.onSubmitPaperVended : this.onSubmit
          )}
          mnemonicValidator={isValidRedemptionMnemonic}
          redemptionCodeValidator={isValidRedemptionKey}
          postVendRedemptionCodeValidator={isValidPaperVendRedemptionKey}
          redemptionType={redemptionType}
          showInputsForDecryptingForceVendedCertificate={
            showInputsForDecryptingForceVendedCertificate
          }
          showInputForDecryptionKey={showInputForDecryptionKey}
          showPassPhraseWidget={showPassPhraseWidget}
          isRedemptionDisclaimerAccepted={environment.isMainnet() || isRedemptionDisclaimerAccepted}
          onAcceptRedemptionDisclaimer={() => acceptRedemptionDisclaimer.trigger()}
          getSelectedWallet={walletId => wallets.getWalletById(walletId)}
        />
      </Layout>
    );
  }
}
