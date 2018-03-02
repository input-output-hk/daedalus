import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import PaperWalletCreateCertificateInstructionsDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificateInstructionsDialog';
import PaperWalletCreateCertificatePasswordChoiceDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificatePasswordChoiceDialog';
import PaperWalletCreateCertificateTemplateChoiceDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificateTemplateChoiceDialog';
import PaperWalletCreateCertificatePrintDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificatePrintDialog';
import PaperWalletCreateCertificateSecuringPasswordDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificateSecuringPasswordDialog';
import PaperWalletCreateCertificateVerificationDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificateVerificationDialog';
import PaperWalletCreateCertificateCompletionDialog from '../../source/renderer/app/components/wallet/settings/paper-wallet-create-certificate-dialogs/PaperWalletCreateCertificateCompletionDialog';

const OPTIONS = ['append', 'home', 'cat', 'dog', 'fish', 'hide', 'hover', 'duck', 'category', 'join', 'paper', 'box', 'tab'];

storiesOf('PaperWallets', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Instructions', () => (
    <div>
      <PaperWalletCreateCertificateInstructionsDialog
        onClose={action('onClose')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Password choice', () => (
    <div>
      <PaperWalletCreateCertificatePasswordChoiceDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Template choice', () => (
    <div>
      <PaperWalletCreateCertificateTemplateChoiceDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Printing the certificate', () => (
    <div>
      <PaperWalletCreateCertificatePrintDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Securing the password', () => (
    <div>
      <PaperWalletCreateCertificateSecuringPasswordDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ))

  .add('Shielded recovery phrase and password verification', () => (
    <div>
      <PaperWalletCreateCertificateVerificationDialog
        suggestedMnemonics={OPTIONS}
        mnemonicValidator={() => {}}
        onPassPhraseChanged={() => {}}
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
        onClear={action('onClear')}
      />
    </div>
  ))

  .add('Completion and link to the address in Cardano Explorer', () => (
    <div>
      <PaperWalletCreateCertificateCompletionDialog
        onClose={action('onClose')}
        onBack={action('onBack')}
        onContinue={action('onContinue')}
      />
    </div>
  ));

