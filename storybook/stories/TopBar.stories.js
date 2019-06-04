// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import SidebarLayout from '../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../source/renderer/app/components/layout/TopBar';
import NodeSyncStatusIcon from '../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../../source/renderer/app/components/widgets/WalletTestEnvironmentLabel';
import { formattedWalletAmount } from '../../source/renderer/app/utils/formatters';

const topBarTestEnv = (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
  >
    <WalletTestEnvironmentLabel network="testnet" />
    <NodeSyncStatusIcon
      daedalusDiagnostics={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction={false}
      isMainnet={false}
    />
  </TopBar>
);

const topBarProductionEnv = (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
  >
    <NodeSyncStatusIcon
      daedalusDiagnostics={{
        isSynced: true,
        syncPercentage: 100,
      }}
      isProduction
      isMainnet
    />
  </TopBar>
);

storiesOf('TopBar', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Test Environment label', () => (
    <SidebarLayout topbar={topBarTestEnv} sidebar={<noscript />} />
  ))

  .add('Production Environment', () => (
    <SidebarLayout topbar={topBarProductionEnv} sidebar={<noscript />} />
  ));
