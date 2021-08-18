// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import moment from 'moment';
import { date, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import stakingDecorator from './_utils/stakingDecorator';

// Screens
import StakingInfo from '../../../source/renderer/app/components/staking/info/StakingInfo';
import StakingEpochs from '../../../source/renderer/app/components/staking/epochs/StakingEpochs';

// Dummy data initialization
import PREVIOUS_EPOCHS from '../../../source/renderer/app/config/stakingPreviousEpoch.dummy.json';
import CURRENT_EPOCHS from '../../../source/renderer/app/config/stakingCurrentEpoch.dummy.json';

const activeItem = 'info';
const threeDaysFromToday = moment().add(3, 'days');
const endDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);
  return new Date(stringTimestamp).toISOString();
};

storiesOf('Decentralization | Info', module)
  .addDecorator(stakingDecorator(activeItem))
  // ====== Stories ======

  .add('Info', () => (
    <StakingInfo
      percentage={number('Percentage', 10, {
        min: 0,
        max: 100,
        step: 1,
        range: true,
      })}
      onLearnMoreClick={action('onLearnMoreClick')}
    />
  ))
  .add('Epochs', () => (
    <StakingEpochs
      currentEpochName={CURRENT_EPOCHS.name}
      currentEpochData={CURRENT_EPOCHS.data}
      currentEpochEndDateTime={endDateTimeKnob(
        'Current Epoch End DateTime',
        new Date(threeDaysFromToday)
      )}
      currentEpochProgress={number(
        'Current Epoch Progress percentage',
        CURRENT_EPOCHS.progress,
        {
          min: 0,
          max: 100,
          step: 1,
          range: true,
        }
      )}
      previousEpochName={PREVIOUS_EPOCHS.name}
      previousEpochData={PREVIOUS_EPOCHS.data}
      isLoading={false}
    />
  ));
