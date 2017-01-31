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

  .add('Certificate not uploaded', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isCertificateUploaded={false}
        isCertificateEncrypted={false}
        onCertificateSelected={action('certificateSelected')}
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ))

  .add('Certificate uploaded - not encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isCertificateUploaded
        isCertificateEncrypted={false}
        onCertificateSelected={action('certificateSelected')}
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ))

  .add('Certificate uploaded - encrypted', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        isCertificateUploaded
        isCertificateEncrypted
        onCertificateSelected={action('certificateSelected')}
        wallets={[
          { value: '', label: '...' },
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ));
