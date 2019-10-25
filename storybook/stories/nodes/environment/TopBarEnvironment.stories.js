// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import SidebarLayout from '../../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../../source/renderer/app/components/layout/TopBar';
import NodeSyncStatusIcon from '../../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../../../../source/renderer/app/components/widgets/WalletTestEnvironmentLabel';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import menuIconClosed from '../../../../source/renderer/app/assets/images/menu-ic.inline.svg';
import NewsFeedIcon from '../../../../source/renderer/app/components/widgets/NewsFeedIcon';

const topBarTestEnv = (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
  >
    <WalletTestEnvironmentLabel network="testnet" />
    <NodeSyncStatusIcon
      networkStatus={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction={false}
      isMainnet={false}
    />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      showDot={false}
    />
  </TopBar>
);

const topBarProductionEnv = (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
  >
    <NodeSyncStatusIcon
      networkStatus={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction
      isMainnet
    />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      showDot={false}
    />
  </TopBar>
);

storiesOf('Nodes|Environment', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Test Environment label', () => (
    <SidebarLayout topbar={topBarTestEnv} sidebar={<noscript />} />
  ))

  .add('Production Environment', () => (
    <SidebarLayout topbar={topBarProductionEnv} sidebar={<noscript />} />
  ));
