import React from 'react';
import moment from 'moment';
import { date, number } from '@storybook/addon-knobs';
// Component
import StakingEpochs from '../../../source/renderer/app/components/staking/epochs/StakingEpochs';
// Dummy data initialization
import PREVIOUS_EPOCHS from '../../../source/renderer/app/config/stakingPreviousEpoch.dummy.json';
import CURRENT_EPOCHS from '../../../source/renderer/app/config/stakingCurrentEpoch.dummy.json';

const threeDaysFromToday = moment().add(3, 'days');

const endDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);
  return new Date(stringTimestamp).toISOString();
};

export const StakingEpochsStory = () => (
  <StakingEpochs
    currentEpochName={CURRENT_EPOCHS.name}
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ pool: { id: string; ranking: number; ticke... Remove this comment to see the full error message
    currentEpochData={CURRENT_EPOCHS.data}
    currentEpochEndDateTime={endDateTimeKnob(
      'Current Epoch End DateTime',
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
    // @ts-ignore ts-migrate(2739) FIXME: Type '{ pool: { id: string; ranking: number; ticke... Remove this comment to see the full error message
    previousEpochData={PREVIOUS_EPOCHS.data}
    isLoading={false}
  />
);
