import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import AdaRedemptionForm from '../app/components/wallet/ada-redemption/AdaRedemptionForm';

storiesOf('AdaRedemptionForm', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('Certificate not selected', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isCertificateSelected={false}
        isCertificateEncrypted={false}
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        redemptionCode=""
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ))

  .add('Certificate selected - not encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isCertificateSelected={true}
        isCertificateEncrypted={false}
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ))

  .add('Certificate selected - encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isSubmitting={false}
        isCertificateSelected={true}
        isCertificateEncrypted={true}
        onCertificateSelected={action('onCertificateSelected')}
        onPassPhraseChanged={action('onPassPhraseChanged')}
        onRedemptionCodeChanged={action('onRedemptionCodeChanged')}
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ));
