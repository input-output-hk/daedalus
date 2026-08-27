import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import type { DappConsentPresentation } from '../../../source/common/ipc/api';
import DappDataSignApproval from '../../../source/renderer/app/components/dapp-consent/DappDataSignApproval';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';

type Request = Extract<DappConsentPresentation, { kind: 'data-sign' }>;

const request = (utf8Preview: string | null): Request => ({
  requestId: 'data-sign-request',
  kind: 'data-sign',
  origin: 'https://example.dapp.test',
  walletName: 'My wallet',
  networkName: 'Preview',
  scopes: ['data-signing'],
  extensions: [95],
  review: {
    address: `60${'11'.repeat(28)}`,
    credentialKind: 'payment',
    payload:
      utf8Preview === null
        ? 'ff00'
        : '5369676e20696e20746f204578616d706c652064417070',
    utf8Preview,
  },
});

storiesOf('dApps / DataSignApproval', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('Safe text preview', () => (
    <DappDataSignApproval
      request={request('Sign in to Example dApp')}
      deciding={false}
      onApprove={action('approve')}
      onReject={action('reject')}
    />
  ))
  .add('Binary payload', () => (
    <DappDataSignApproval
      request={request(null)}
      deciding={false}
      onApprove={action('approve')}
      onReject={action('reject')}
    />
  ))
  .add('DRep governance payload', () => (
    <DappDataSignApproval
      request={{
        ...request('Governance authorization'),
        scopes: ['governance-data-signing'],
        review: {
          ...request('Governance authorization').review,
          address: '33'.repeat(28),
          credentialKind: 'drep',
        },
      }}
      deciding={false}
      onApprove={action('approve DRep signature')}
      onReject={action('reject')}
    />
  ));
