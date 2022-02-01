import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import StakingInfoCountdown from '../../../source/renderer/app/components/staking/info/StakingInfoCountdown';
import NodeSyncStatusIcon from '../../../source/renderer/app/components/widgets/NodeSyncStatusIcon';
import NewsFeedIcon from '../../../source/renderer/app/components/widgets/NewsFeedIcon';
import TadaButton from '../../../source/renderer/app/components/widgets/TadaButton';

storiesOf('Decentralization | Countdown', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Countdown party', () => {
    const isAlonzoActivated = boolean('isAlonzoActivated', false);
    const date = isAlonzoActivated
      ? new Date().getTime() - 100000000
      : new Date().getTime() + 100000000;
    const startDateTime = new Date(date).toISOString();
    return (
      <div>
        <TopBar
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            isProduction
            isMainnet
            hasTadaIcon={isAlonzoActivated}
          />
          {isAlonzoActivated && (
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
          onLearnMoreClick={action('onLearnMoreClick')}
          startDateTime={startDateTime}
          onSetStakingInfoWasOpen={action('onSetStakingInfoWasOpen')}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          isAnimating={boolean('isAnimating', false)}
          isAlonzoActivated={boolean('isAlonzoActivated', false)}
          stakingInfoWasOpen={boolean('stakingInfoWasOpen', false)}
          onStartStakingInfoAnimation={action('onStartStakingInfoAnimation')}
          onStopStakingInfoAnimation={action('onStopStakingInfoAnimation')}
        />
      </div>
    );
  });
