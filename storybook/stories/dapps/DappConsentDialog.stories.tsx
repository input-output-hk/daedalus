import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import DappConsentDialog from '../../../source/renderer/app/components/dapp-consent/DappConsentDialog';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';

storiesOf('dApps / Consent', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('CIP-95 key disclosure', () => (
    <DappConsentDialog
      request={{
        requestId: 'cip95-disclosure',
        kind: 'key-disclosure',
        origin: 'https://example.dapp.test',
        walletName: 'My wallet',
        networkName: 'Preview',
        scopes: ['governance-key-disclosure'],
        extensions: [95],
      }}
      deciding={false}
      onApprove={action('approve')}
      onReject={action('reject')}
    />
  ));
