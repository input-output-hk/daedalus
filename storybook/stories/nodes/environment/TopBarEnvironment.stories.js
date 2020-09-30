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
import { isShelleyTestnetTheme } from '../../_support/utils';

const topBarTestEnv = currentTheme => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
  >
    <WalletTestEnvironmentLabel network="testnet" />
    <NodeSyncStatusIcon
      isSynced
      syncPercentage={100}
      isProduction={false}
      isMainnet={false}
    />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

const topBarItnEnv = currentTheme => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
  >
    <WalletTestEnvironmentLabel network="itn_rewards_v1" />
    <NodeSyncStatusIcon
      isSynced
      syncPercentage={100}
      isProduction={false}
      isMainnet={false}
    />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

const topBarProductionEnv = currentTheme => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
  >
    <NodeSyncStatusIcon isSynced syncPercentage={100} isProduction isMainnet />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

storiesOf('Nodes|Environment', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  // ====== Stories ======
  .add('Testnet', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarTestEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  .add('Incentivized Testnet', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarItnEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  .add('Production', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarProductionEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ));
