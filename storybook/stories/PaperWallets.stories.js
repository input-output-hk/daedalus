// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import wordlist from 'bip39/wordlists/english';
import StoryDecorator from './support/StoryDecorator';
import InstructionsDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/InstructionsDialog';
import PrintDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/PrintDialog';
import SecuringPasswordDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import VerificationDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/VerificationDialog';
import CompletionDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/CompletionDialog';

storiesOf('PaperWallets', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Instructions', () => (
    <div>
      <InstructionsDialog
        inProgress={false}
        onOpenExternalLink={action('onOpenExternalLink')}
        onPrint={action('onPrint')}
        onClose={action('onClose')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Printing the certificate', () => (
    <div>
      <PrintDialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Securing the password', () => (
    <div>
      <SecuringPasswordDialog
        additionalMnemonics="flugenheimer"
        onContinue={action('onContinue')}
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('Shielded recovery phrase and password verification', () => (
    <div>
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

  .add('Completion and link to the address in Cardano Explorer', () => (
    <div>
      <CompletionDialog
        walletCertificateAddress="WalletCertificateAddress"
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
        onOpenExternalLink={action('onOpenExternalLink')}
      />
    </div>
  ));

