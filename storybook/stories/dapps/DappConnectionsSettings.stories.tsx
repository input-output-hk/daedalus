import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import DappConnectionsSettings from '../../../source/renderer/app/components/settings/categories/DappConnectionsSettings';
import type { DappConnectionRow } from '../../../source/renderer/app/components/settings/categories/DappConnectionsSettings';
import StoryDecorator from '../_support/StoryDecorator';

const connection: DappConnectionRow = {
  walletName: 'Main wallet',
  grant: {
    schemaVersion: 1,
    origin: 'https://example.com',
    walletId: 'wallet-1',
    networkGenesis: 'genesis-1',
    networkMagic: 1,
    readScopes: ['connection', 'read', 'governance-key-disclosure'],
    enabledExtensionScopes: [95],
    launch: { kind: 'diagnostics' },
    grantedAt: '2026-08-27T00:00:00.000Z',
  },
};
const callbacks = {
  onDisconnect: action('disconnect'),
  onForget: action('forget'),
  onRevoke: action('revoke scope'),
  onRepair: action('repair'),
};

storiesOf('dApps / Connection settings', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('saved connection', () => (
    <DappConnectionsSettings
      {...callbacks}
      connections={[connection]}
      corrupt={false}
      loading={false}
      failed={false}
    />
  ))
  .add('empty', () => (
    <DappConnectionsSettings
      {...callbacks}
      connections={[]}
      corrupt={false}
      loading={false}
      failed={false}
    />
  ))
  .add('corrupt repair', () => (
    <DappConnectionsSettings
      {...callbacks}
      connections={[]}
      corrupt
      loading={false}
      failed={false}
    />
  ));
