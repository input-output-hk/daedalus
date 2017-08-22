import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import SidebarLayout from '../app/components/layout/SidebarLayout';
import TopBar from '../app/components/layout/TopBar';
import NodeSyncStatusIcon from '../app/components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../app/components/widgets/WalletTestEnvironmentLabel';

const topBarTestEnv = (
  <TopBar>
    <WalletTestEnvironmentLabel version={0.5} />
    <NodeSyncStatusIcon
      networkStatus={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction={false}
    />
  </TopBar>
);

const topBarProductionEnv = (
  <TopBar>
    <NodeSyncStatusIcon
      networkStatus={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction
    />
  </TopBar>
);

storiesOf('TopBar', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

   // ====== Stories ======

  .add('Test Environment label', () => (
    <SidebarLayout
      topbar={topBarTestEnv}
      sidebar={<noscript />}
    />
  ))

  .add('Production Environment', () => (
    <SidebarLayout
      topbar={topBarProductionEnv}
      sidebar={<noscript />}
    />
  ));
