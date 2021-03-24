// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

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

  .add('Countdown party', () => {
    const percentage = number('percentage', 98, {
      range: true,
      min: 0,
      max: 100,
      step: 1,
    });
    const epochNumber = number('epochNumber', 257);
    const isFullyDecentralized = percentage === 100;
    const date = isFullyDecentralized
      ? new Date().getTime() - 100000000
      : new Date().getTime() + 100000000;
    const countdownDate = new Date(date).toISOString();
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
            hasTadaIcon={isFullyDecentralized}
          />
          {isFullyDecentralized && (
            <CountdownPartyIcon
              onIconClick={action('onIconClick')}
              shouldAnimate={boolean('shouldAnimate', false)}
            />
          )}

          <NewsFeedIcon
            onNewsFeedIconClick={action('onNewsFeedIconClick')}
            hasNotification={false}
            hasUpdate={false}
          />
          <FullyDecentralizedEffect isActive={isFullyDecentralized} />
        </TopBar>
        <StakingInfo
          percentage={percentage}
          onLearnMoreClick={action('onLearnMoreClick')}
          epochNumber={epochNumber}
          date={countdownDate}
        />
      </div>
    );
  });
