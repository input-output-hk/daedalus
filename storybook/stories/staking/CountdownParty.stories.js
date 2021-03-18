// @flow
import React from 'react';
import { storiesOf, addDecorator } from '@storybook/react';
import { withKnobs, number, boolean, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { observable, runInAction } from 'mobx';

import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import StakingInfo from '../../../source/renderer/app/components/staking/info/StakingInfo';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import CountdownPartyIcon from '../../../source/renderer/app/components/widgets/CountdownPartyIcon';
import FullyDecentralizedEffect from '../../../source/renderer/app/components/widgets/FullyDecentralizedEffect';

storiesOf('Decentralization | Countdown', module)
  // .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Countdown party', (props) => {
    console.log('props', props);

    const isFullyDecentralized = boolean(
      'isFullyDecentralized',
      false,
      'Party mode'
    );

    return (
      <div>
        <TopBar
          onToggleSidebar={action('onToggleSidebar')}
          formattedWalletAmount={1.0}
          currentRoute=""
          showSubMenuToggle
          showSubMenus
          onTransferFunds={action('onTransferFunds')}
          onWalletAdd={action('onWalletAdd')}
          hasRewardsWallets={false}
          isShelleyActivated
          isFullyDecentralized={isFullyDecentralized}
        >
          <NodeSyncStatusIcon
            isSynced
            syncPercentage={100}
            isProduction
            isMainnet
            isFullyDecentralized={isFullyDecentralized}
          />
          {isFullyDecentralized && (
            <CountdownPartyIcon onIconClick={action('onIconClick')} />
          )}

          <NewsFeedIcon
            onNewsFeedIconClick={action('onNewsFeedIconClick')}
            hasNotification={false}
            hasUpdate={false}
          />
          <FullyDecentralizedEffect
            isActive={isFullyDecentralized}
            effect={select(
              'Effect',
              { confett: 'confett', fireworks: 'fireworks' },
              'fireworks',
              'Party mode'
            )}
            containerSelector=".TopBar_topBar"
          />
        </TopBar>
        <StakingInfo
          percentage={number('percentage', 50)}
          onLearnMoreClick={action('onLearnMoreClick')}
          isFullyDecentralized={isFullyDecentralized}
        />
      </div>
    );
  });
