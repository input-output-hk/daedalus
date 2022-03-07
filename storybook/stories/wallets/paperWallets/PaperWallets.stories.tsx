import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import wordlist from '../../../../source/common/config/crypto/valid-words.en';
import InstructionsDialog from '../../../../source/renderer/app/components/wallet/paper-wallet-certificate/InstructionsDialog';
import PrintDialog from '../../../../source/renderer/app/components/wallet/paper-wallet-certificate/PrintDialog';
import SecuringPasswordDialog from '../../../../source/renderer/app/components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import VerificationDialog from '../../../../source/renderer/app/components/wallet/paper-wallet-certificate/VerificationDialog';
import CompletionDialog from '../../../../source/renderer/app/components/wallet/paper-wallet-certificate/CompletionDialog';

storiesOf('Wallets|Paper Wallets', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Instructions', () => (
    <div>
      <InstructionsDialog
        inProgress={false}
        onOpenExternalLink={action('onOpenExternalLink')}
        onPrint={action('onPrint')}
        onClose={action('onClose')}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onContinue={action('onContinue')}
      />
    </div>
  ))
  .add('Verify', () => (
    <div>
      <PrintDialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
      />
    </div>
  ))
  .add('Complete Mnemonics', () => (
    <div>
      <SecuringPasswordDialog
        additionalMnemonics="flugenheimer"
        onContinue={action('onContinue')}
        onClose={action('onClose')}
      />
    </div>
  ))
  .add('Confirm Mnemonics', () => (
    <div>
      {/* @ts-ignore ts-migrate(2769) FIXME: No overload matches this call. */}
      <VerificationDialog
        walletCertificatePassword="flugenheimer"
        walletCertificateRecoveryPhrase={wordlist}
        additionalMnemonicWords={wordlist}
        suggestedMnemonics={wordlist}
        onContinue={action('onContinue')}
        onClear={action('onClear')}
        onClose={action('onClose')}
      />
    </div>
  ))
  .add('Completion', () => (
    <div>
      <CompletionDialog
        walletCertificateAddress="WalletCertificateAddress"
        onClose={action('onClose')}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onBack={action('onBack')}
        onContinue={action('onContinue')}
        onOpenExternalLink={action('onOpenExternalLink')}
        copyAddressNotificationDuration={1}
      />
    </div>
  ));
