// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { date, number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import stakingDecorator from './_utils/stakingDecorator';

import StakingCountdown from '../../../source/renderer/app/components/staking/countdown/StakingCountdown';
import StakingInfoCountdown from '../../../source/renderer/app/components/staking/info/StakingInfoCountdown';

const activeItem = 'info';
const defaultStartDateTime = new Date();
defaultStartDateTime.setDate(defaultStartDateTime.getDate() + 2);
const startDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);
  return new Date(stringTimestamp).toISOString();
};

storiesOf('Decentralization | Countdown', module)
  .addDecorator(stakingDecorator(activeItem))
  .add('Decentralization Countdown', () => (
    <div>
      <StakingCountdown
        startDateTime={startDateTimeKnob(
          'Decentralization Start DateTime',
          defaultStartDateTime
        )}
        onLearnMoreClick={action('onLearnMoreClick')}
      />
    </div>
  ))
  .add('Countdown info', () => {
    const percentage = number('percentage', 98, {
      range: true,
      min: 0,
      max: 100,
      step: 1,
    });
    const epochNumber = number('epochNumber', 257);
    const isFullyDecentralized = percentage === 100;
    const epochDate = isFullyDecentralized
      ? new Date().getTime() - 100000000
      : new Date().getTime() + 100000000;
    const epochStart = new Date(epochDate).toISOString();
    return (
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
    );
  });
