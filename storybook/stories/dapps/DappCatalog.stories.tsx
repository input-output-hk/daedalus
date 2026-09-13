import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { injectIntl } from 'react-intl';
import { getDappCatalogPresentation } from '../../../source/common/config/dappCatalog';
import type { Network } from '../../../source/common/types/environment.types';
import DappCatalog from '../../../source/renderer/app/components/dapp/DappCatalog';
import type { DappCatalogProps } from '../../../source/renderer/app/components/dapp/DappCatalog';
import type { Intl } from '../../../source/renderer/app/types/i18nTypes';
import StoryDecorator from '../_support/StoryDecorator';

type CatalogStoryProps = {
  network: Network;
} & Omit<DappCatalogProps, 'entries'>;

const CatalogStory = injectIntl(
  ({ intl, network, ...props }: CatalogStoryProps & { intl: Intl }) => (
    <div
      style={{
        background: 'var(--theme-main-body-background-color)',
        minHeight: '100vh',
      }}
    >
      <DappCatalog
        entries={getDappCatalogPresentation(network, false).map((entry) => ({
          id: entry.id,
          name: intl.formatMessage({
            id: entry.nameMessageId,
            defaultMessage: entry.nameMessageId,
            description: '',
          }),
          description: intl.formatMessage({
            id: entry.descriptionMessageId,
            defaultMessage: entry.descriptionMessageId,
            description: '',
          }),
          iconAsset: entry.iconAsset,
        }))}
        {...props}
      />
    </div>
  )
);

const onLaunch = action('launch');
const onClose = action('close');

storiesOf('dApps / Preferred catalog', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('ready', () => (
    <CatalogStory
      network="mainnet"
      available
      ready
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('preprod', () => (
    <CatalogStory
      network="preprod"
      available
      ready
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('not ready', () => (
    <CatalogStory
      network="mainnet"
      available
      ready={false}
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('open', () => (
    <CatalogStory
      network="mainnet"
      available
      ready
      isOpen
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ))
  .add('unavailable', () => (
    <CatalogStory
      network="mainnet"
      available={false}
      ready={false}
      isOpen={false}
      isLaunching={false}
      onLaunch={onLaunch}
      onClose={onClose}
    />
  ));
