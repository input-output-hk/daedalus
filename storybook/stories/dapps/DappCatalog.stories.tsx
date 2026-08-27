import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import DappCatalog from '../../../source/renderer/app/components/dapp/DappCatalog';
import type { DappCatalogEntry } from '../../../source/renderer/app/components/dapp/DappCatalog';
import StoryDecorator from '../_support/StoryDecorator';

const entries: DappCatalogEntry[] = [
  {
    id: 'exchange',
    name: 'Example Exchange',
    description: 'Swap Cardano native assets from a locally bundled dApp.',
    iconAsset: 'cardano',
  },
  {
    id: 'marketplace',
    name: 'Example Marketplace',
    description: 'Browse a locally bundled demonstration marketplace.',
    iconAsset: 'cardano',
  },
];

const onLaunch = action('launch');
const onClose = action('close');

storiesOf('dApps / Preferred catalog', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('ready', () => (
    <DappCatalog
      entries={entries}
      available
      ready
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('not ready', () => (
    <DappCatalog
      entries={entries}
      available
      ready={false}
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('open', () => (
    <DappCatalog
      entries={entries}
      available
      ready
      isOpen
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('unavailable', () => (
    <DappCatalog
      entries={entries}
      available={false}
      ready={false}
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ));
