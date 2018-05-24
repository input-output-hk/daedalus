// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import wordlist from 'bip39/wordlists/english';
import StoryDecorator from './support/StoryDecorator';
import AdaRedemptionForm from '../../source/renderer/app/components/wallet/ada-redemption/AdaRedemptionForm';
import AdaRedemptionChoices from '../../source/renderer/app/components/wallet/ada-redemption/AdaRedemptionChoices';
import { ADA_REDEMPTION_TYPES } from '../../source/renderer/app/types/redemptionTypes';

storiesOf('AdaRedemptionForm', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Ada redemption choices', () => (
    <div>
      <AdaRedemptionChoices
        activeChoice="forceVended"
        onSelectChoice={action('selectChoice')}
      />
    </div>
  ))

  .add('Certificate not selected', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isRedemptionDisclaimerAccepted
        isCertificateSelected={false}
        isCertificateEncrypted={false}
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        onChooseRedemptionType={action('onChooseRedemptionType')}
        redemptionCode=""
        redemptionType={ADA_REDEMPTION_TYPES.REGULAR}
        getSelectedWallet={() => ({})}
        wallets={[
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
        suggestedMnemonics={wordlist}
        showPassPhraseWidget={false}
        isCertificateInvalid={false}
        showInputsForDecryptingForceVendedCertificate={false}
        redemptionCodeValidator={() => {}}
        postVendRedemptionCodeValidator={() => {}}
        onRemoveCertificate={() => {}}
        onEmailChanged={() => {}}
        onAdaPasscodeChanged={() => {}}
        onAdaAmountChanged={() => {}}
        onAcceptRedemptionDisclaimer={() => {}}
        mnemonicValidator={() => {}}
        error={null}
      />
    </div>
  ))

  .add('Certificate selected - not encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isRedemptionDisclaimerAccepted
        isCertificateSelected
        isCertificateEncrypted={false}
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        onChooseRedemptionType={action('onChooseRedemptionType')}
        redemptionCode=""
        redemptionType={ADA_REDEMPTION_TYPES.REGULAR}
        getSelectedWallet={() => ({})}
        wallets={[
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
        suggestedMnemonics={wordlist}
        showPassPhraseWidget={false}
        isCertificateInvalid={false}
        showInputsForDecryptingForceVendedCertificate={false}
        redemptionCodeValidator={() => {}}
        postVendRedemptionCodeValidator={() => {}}
        onRemoveCertificate={() => {}}
        onEmailChanged={() => {}}
        onAdaPasscodeChanged={() => {}}
        onAdaAmountChanged={() => {}}
        onAcceptRedemptionDisclaimer={() => {}}
        mnemonicValidator={() => {}}
        error={null}
      />
    </div>
  ))

  .add('Certificate selected - encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isRedemptionDisclaimerAccepted
        isCertificateSelected
        isCertificateEncrypted
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        onChooseRedemptionType={action('onChooseRedemptionType')}
        redemptionCode=""
        redemptionType={ADA_REDEMPTION_TYPES.REGULAR}
        getSelectedWallet={() => ({})}
        wallets={[
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
        suggestedMnemonics={wordlist}
        showPassPhraseWidget={false}
        isCertificateInvalid={false}
        showInputsForDecryptingForceVendedCertificate={false}
        redemptionCodeValidator={() => {}}
        postVendRedemptionCodeValidator={() => {}}
        onRemoveCertificate={() => {}}
        onEmailChanged={() => {}}
        onAdaPasscodeChanged={() => {}}
        onAdaAmountChanged={() => {}}
        onAcceptRedemptionDisclaimer={() => {}}
        mnemonicValidator={() => {}}
        error={null}
      />
    </div>
  ));
