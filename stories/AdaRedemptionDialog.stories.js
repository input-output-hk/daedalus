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

  .add('default', () => (
    <div>
      <AdaRedemptionForm
        onSubmit={action('submit')}
        onCertificateSelected={action('certificateSelected')}
        wallets={[
          { value: 'wallet-1', label: 'First Wallet' },
          { value: 'wallet-2', label: 'Second Wallet' },
          { value: 'wallet-3', label: 'Third Wallet' },
        ]}
      />
    </div>
  ))
