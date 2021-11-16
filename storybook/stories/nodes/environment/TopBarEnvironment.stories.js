// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import classNames from 'classnames';
import StoryDecorator from '../../_support/StoryDecorator';
import SidebarLayout from '../../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../../source/renderer/app/components/layout/TopBar';
import topBarStyles from '../../../../source/renderer/app/components/layout/TopBar.scss';
import NodeSyncStatusIcon from '../../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import TadaButton from '../../../../source/renderer/app/components/widgets/TadaButton';
import WalletTestEnvironmentLabel from '../../../../source/renderer/app/components/widgets/WalletTestEnvironmentLabel';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
import menuIconClosed from '../../../../source/renderer/app/assets/images/menu-ic.inline.svg';
import NewsFeedIcon from '../../../../source/renderer/app/components/widgets/NewsFeedIcon';
import DiscreetToggle from '../../../../source/renderer/app/features/discreet-mode/ui/discreet-toggle/DiscreetToggle';
import { isShelleyTestnetTheme } from '../../_support/utils';

const topBarTestEnv = (currentTheme) => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
    isAlonzoActivated={boolean('isAlonzoActivated', false)}
  >
    <WalletTestEnvironmentLabel network="testnet" />
    <NodeSyncStatusIcon
      isSynced
      syncPercentage={100}
      isProduction={false}
      isMainnet={false}
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggle
      className={classNames(
        topBarStyles.discreetModeToggle,
        topBarStyles.hasTadaIcon
      )}
      onToggle={action('onDiscreetModeToggle')}
    />
    <TadaButton onClick={action('onClickTadaButton')} shouldAnimate />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

const topBarStagingEnv = (currentTheme) => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
    isAlonzoActivated={boolean('isAlonzoActivated', false)}
  >
    <WalletTestEnvironmentLabel network="staging" />
    <NodeSyncStatusIcon
      isSynced
      syncPercentage={100}
      isProduction={false}
      isMainnet={false}
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggle
      className={classNames(
        topBarStyles.discreetModeToggle,
        topBarStyles.hasTadaIcon
      )}
      onToggle={action('onDiscreetModeToggle')}
    />
    <TadaButton onClick={action('onClickTadaButton')} shouldAnimate />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

const topBarProductionEnv = (currentTheme) => (
  <TopBar
    formattedWalletAmount={formattedWalletAmount}
    currentRoute=""
    showSubMenuToggle={false}
    leftIcon={menuIconClosed}
    isShelleyActivated={isShelleyTestnetTheme(currentTheme)}
    isAlonzoActivated={boolean('isAlonzoActivated', false)}
  >
    <NodeSyncStatusIcon
      isSynced
      syncPercentage={100}
      isProduction
      isMainnet
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggle
      className={classNames(
        topBarStyles.discreetModeToggle,
        topBarStyles.hasTadaIcon
      )}
      onToggle={action('onDiscreetModeToggle')}
    />
    <TadaButton onClick={action('onClickTadaButton')} shouldAnimate />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

storiesOf('Nodes|Environment', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  // ====== Stories ======
  .add('Testnet', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarTestEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  .add('Staging', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarStagingEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  .add('Production', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarProductionEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ));
