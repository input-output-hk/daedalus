import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import classNames from 'classnames';
import StoryDecorator from '../../_support/StoryDecorator';
import StoryProvider from '../../_support/StoryProvider';
import SidebarLayout from '../../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../../source/renderer/app/components/layout/TopBar';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../source/renderer/ap... Remove this comment to see the full error message
import topBarStyles from '../../../../source/renderer/app/components/layout/TopBar.scss';
import NodeSyncStatusIcon from '../../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import TadaButton from '../../../../source/renderer/app/components/widgets/TadaButton';
import WalletTestEnvironmentLabel from '../../../../source/renderer/app/components/widgets/WalletTestEnvironmentLabel';
import { formattedWalletAmount } from '../../../../source/renderer/app/utils/formatters';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../source/renderer/ap... Remove this comment to see the full error message
import menuIconClosed from '../../../../source/renderer/app/assets/images/menu-ic.inline.svg';
import NewsFeedIcon from '../../../../source/renderer/app/components/widgets/NewsFeedIcon';
import DiscreetToggleTopBar from '../../../../source/renderer/app/features/discreet-mode/ui/discreet-toggle-top-bar/DiscreetToggleTopBar';
import { isShelleyTestnetTheme } from '../../_support/utils';

const topBarTestEnv = (currentTheme) => (
  <TopBar
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isProduction={false}
      isMainnet={false}
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggleTopBar hasTadaIcon />
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
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isProduction={false}
      isMainnet={false}
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggleTopBar hasTadaIcon />
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
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      isProduction
      isMainnet
      hasTadaIcon
    />
    <span
      className={classNames(topBarStyles.rectangle, topBarStyles.hasTadaIcon)}
    />
    <DiscreetToggleTopBar hasTadaIcon />
    <TadaButton onClick={action('onClickTadaButton')} shouldAnimate />
    <NewsFeedIcon
      onNewsFeedIconClick={action('onNewsFeedIconClick')}
      hasNotification={false}
      hasUpdate={false}
    />
  </TopBar>
);

storiesOf('Nodes|Environment', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs) // ====== Stories ======
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Testnet', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarTestEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Staging', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarStagingEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(props: {    currentTheme: strin... Remove this comment to see the full error message
  .add('Production', (props: { currentTheme: string }) => (
    <SidebarLayout
      topbar={topBarProductionEnv(props.currentTheme)}
      sidebar={<noscript />}
    />
  ));
