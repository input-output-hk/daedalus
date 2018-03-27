import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import wordlist from 'bip39/wordlists/english';
import StoryDecorator from './support/StoryDecorator';
import InstructionsDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/InstructionsDialog';
import PasswordChoiceDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/PasswordChoiceDialog';
import TemplateChoiceDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/TemplateChoiceDialog';
import PrintDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/PrintDialog';
import SecuringPasswordDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import VerificationDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/VerificationDialog';
// import CompletionDialog from '../../source/renderer/app/components/wallet/paper-wallet-certificate/CompletionDialog';

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
        onClose={action('onClose')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Password choice', () => (
    <div>
      <PasswordChoiceDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Template choice', () => (
    <div>
      <TemplateChoiceDialog
        onPrint={action('onPrint')}
      />
    </div>
  ))

  .add('Printing the certificate', () => (
    <div>
      <PrintDialog
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Securing the password', () => (
    <div>
      <SecuringPasswordDialog
        walletCertificatePassword="flugenheimer"
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Shielded recovery phrase and password verification', () => (
    <div>
      <VerificationDialog
        walletCertificatePassword="flugenheimer"
        walletCertificateRecoveryPhrase={wordlist}
        suggestedMnemonics={wordlist}
        onContinue={action('onContinue')}
        onClear={action('onClear')}
      />
    </div>
  ));

  // Commented due to 'fs' error
  // .add('Completion and link to the address in Cardano Explorer', () => (
  //   <div>
  //     <CompletionDialog
  //       onClose={action('onClose')}
  //       onBack={action('onBack')}
  //       onContinue={action('onContinue')}
  //     />
  //   </div>
  // ));

