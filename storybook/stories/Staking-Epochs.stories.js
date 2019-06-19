// @flow
import React from 'react';
import { date, number } from '@storybook/addon-knobs';

// Component
import StakingEpochs from '../../source/renderer/app/components/staking/epochs/StakingEpochs';

// Dummy data initialization
const currentEpochName = 'Epoch 94';
const currentEpochData = [
  {
    pool: {
      title: 'Help the USA Cats',
      category: 'CATS',
    },
    slotsElected: [1],
  },
  {
    pool: {
      title: 'Cardano Foundation 1',
      category: 'CF1',
    },
    slotsElected: [0.9],
  },
  {
    pool: {
      title: 'Blush Pool 1',
      category: 'BLS1',
    },
    slotsElected: [0.78],
  },
  {
    pool: {
      title: 'Blush Pool 2',
      category: 'BLS2',
    },
    slotsElected: [0.5],
  },
  {
    pool: {
      title: 'Micro Mining',
      category: 'MNG',
    },
    slotsElected: [0.17],
  },
  {
    pool: {
      title: 'Saint-Petersburg Academy of Programming',
      category: 'SPBA',
    },
    slotsElected: [0.08],
  },
];
const currentEpochProgress = 15;
const currentEpochEndDateTime = new Date('2019-07-15');
const endDateTimeKnob = (name, defaultValue) => {
  const stringTimestamp = date(name, defaultValue);

  return new Date(stringTimestamp).toISOString();
};

const previousEpochName = 'Epoch 93';
const previousEpochData = [
  {
    pool: {
      title: 'Help the USA Cats',
      category: 'CATS',
    },
    slotsElected: [216, 1],
    performance: [216, 216, 100],
    sharedRewards: [194, 216],
  },
  {
    pool: {
      title: 'Cardano Foundation 1',
      category: 'CF1',
    },
    slotsElected: [193, 0.9],
    performance: [193, 193, 100],
    sharedRewards: [193, 193],
  },
  {
    pool: {
      title: 'Blush Pool 1',
      category: 'BLS1',
    },
    slotsElected: [160, 0.78],
    performance: [158, 160, 98],
    sharedRewards: [79, 158],
  },
  {
    pool: {
      title: 'Blush Pool 2',
      category: 'BLS2',
    },
    slotsElected: [101, 0.5],
    performance: [101, 101, 100],
    sharedRewards: [65, 101],
  },
  {
    pool: {
      title: 'Micro Mining',
      category: 'MNG',
    },
    slotsElected: [25, 0.17],
    performance: [2, 25, 8],
    sharedRewards: [0, 2],
  },
  {
    pool: {
      title: 'Saint-Petersburg Academy of Programming',
      category: 'SPBA',
    },
    slotsElected: [11, 0.08],
    performance: [8, 11, 72],
    sharedRewards: [1, 8],
  },
];

export const StakingEpochsStory = () => (
  <StakingEpochs
    currentEpochName={currentEpochName}
    currentEpochData={currentEpochData}
    currentEpochEndDateTime={endDateTimeKnob(
      'Current Epoch End DateTime',
      currentEpochEndDateTime
    )}
    currentEpochProgress={number(
      'Current Epoch Progress percentage',
      currentEpochProgress,
      {
        min: 0,
        max: 100,
        step: 1,
        range: true,
      }
    )}
    previousEpochName={previousEpochName}
    previousEpochData={previousEpochData}
    isLoading={false}
  />
);
