// @flow
import React from 'react';
// import { observable, runInAction } from 'mobx';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select, boolean, number } from '@storybook/addon-knobs';
// import {
//   DEVELOPMENT,
//   TESTNET,
//   STAGING,
// } from '../../../source/common/types/environment.types';
import StoryDecorator from '../_support/StoryDecorator';
// import { isShelleyTestnetTheme } from '../_support/utils';
// import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
// import {
//   CATEGORIES_WITH_DELEGATION_COUNTDOWN,
//   CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN,
// } from '../../../source/renderer/app/config/sidebarConfig';

import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import NodeConnectionIcon from '../../../source/renderer/app/components/widgets/NodeConnectionIcon';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import TadaButton from '../../../source/renderer/app/components/widgets/TadaButton';
import WalletTestEnvironmentLabel from '../../../source/renderer/app/components/widgets/WalletTestEnvironmentLabel';
// import type { InjectedProps } from '../types/injectedPropsType';
import menuIconOpened from '../../../source/renderer/app/assets/images/menu-opened-ic.inline.svg';
import menuIconClosed from '../../../source/renderer/app/assets/images/menu-ic.inline.svg';
// import { matchRoute } from '../utils/routing';
// import { ROUTES } from '../routes-config';

storiesOf('Navigation|TopBar', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  // ====== Stories ======
  .add('No Category', () => {
    const connectionState = select(
      'Connection',
      {
        Connected: 'connected',
        Connecting: 'connecting',
        Disconnected: 'disConnected',
      },
      'connected'
    );
    const isMainnet = boolean('isMainnet', true);
    const network = isMainnet ? 'mainnet' : 'testnet';
    const testnetLabel = !isMainnet ? (
      <WalletTestEnvironmentLabel network={network} />
    ) : null;
    const hasTadaIcon = boolean('hasTadaIcon');
    const nodeIcon =
      connectionState === 'connected' ? (
        <NodeSyncStatusIcon
          isSynced={boolean('isSynced', true, 'NodeSyncStatusIcon')}
          syncPercentage={number(
            'syncPercentage',
            100,
            {},
            'NodeSyncStatusIcon'
          )}
          hasTadaIcon={hasTadaIcon}
        />
      ) : (
        <NodeConnectionIcon
          hasTadaIcon={hasTadaIcon}
          isConnecting={connectionState === 'connecting'}
        />
      );

    return (
      <TopBar
        onLeftIconClick={action('onLeftIconClick')}
        leftIcon={
          boolean('Menu is open', true) ? menuIconOpened : menuIconClosed
        }
        hasRewardsWallets={boolean('hasRewardsWallets', false)}
        isShelleyActivated={boolean('isShelleyActivated', false)}
        onTransferFunds={action('onTransferFunds')}
        onWalletAdd={action('onWalletAdd')}
        onLearnMore={action('onLearnMore')}
      >
        {testnetLabel}
        {nodeIcon}
        {hasTadaIcon && (
          <TadaButton
            onClick={action('onClick')}
            shouldAnimate={boolean('shouldAnimate', false)}
          />
        )}
        <NewsFeedIcon
          onNewsFeedIconClick={action('onNewsFeedIconClick')}
          hasNotification={boolean('hasNotification', false)}
          hasUpdate={boolean('hasUpdate', false)}
        />
      </TopBar>
    );
  });
