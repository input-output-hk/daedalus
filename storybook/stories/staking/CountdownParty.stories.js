// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';

import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import StakingInfoCountdown from '../../../source/renderer/app/components/staking/info/StakingInfoCountdown';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import TadaButton from '../../../source/renderer/app/components/widgets/TadaButton';

storiesOf('Decentralization | Countdown', module)
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
    const epochStart = new Date(date).toISOString();
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
          isDecentralizedEffectActive
        >
          <NodeSyncStatusIcon
            isSynced
            syncPercentage={100}
            isProduction
            isMainnet
            hasTadaIcon={isFullyDecentralized}
          />
          {isFullyDecentralized && (
            <TadaButton
              onClick={action('onIconClick')}
              shouldAnimate={boolean('shouldAnimate', false)}
            />
          )}

          <NewsFeedIcon
            onNewsFeedIconClick={action('onNewsFeedIconClick')}
            hasNotification={false}
            hasUpdate={false}
          />
        </TopBar>
        <StakingInfoCountdown
          percentage={percentage}
          onLearnMoreClick={action('onLearnMoreClick')}
          epoch={{
            epochNumber,
            epochStart,
          }}
          onSetStakingInfoWasOpen={action('onSetStakingInfoWasOpen')}
          isAnimating={boolean('isAnimating', false)}
          isFullyDecentralized={boolean('isFullyDecentralized', false)}
          stakingInfoWasOpen={boolean('stakingInfoWasOpen', false)}
          onStartStakingInfoAnimation={action('onStartStakingInfoAnimation')}
          onStopStakingInfoAnimation={action('onStopStakingInfoAnimation')}
        />
      </div>
    );
  });
