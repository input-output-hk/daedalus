import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import AdaRedemptionDialog from '../app/components/wallet/ada-redemption/AdaRedemptionDialog';

storiesOf('AdaRedemptionDialog', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div>
      <AdaRedemptionDialog
        onSubmit={action('submit')}
        onCancel={action('cancel')}
        onCertificateSelected={action('certificateSelected')}
      />
    </div>
  ))
