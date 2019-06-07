// @flow
import React from 'react';

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
    slotsElected: '1%',
  },
  {
    pool: {
      title: 'Cardano Foundation 1',
      category: 'CF1',
    },
    slotsElected: '0.9%',
  },
  {
    pool: {
      title: 'Blush Pool 1',
      category: 'BLS1',
    },
    slotsElected: '0.78%',
  },
  {
    pool: {
      title: 'Blush Pool 2',
      category: 'BLS2',
    },
    slotsElected: '0.5%',
  },
  {
    pool: {
      title: 'Micro Mining',
      category: 'MNG',
    },
    slotsElected: '0.17%',
  },
  {
    pool: {
      title: 'Saint-Petersburg Academy of Programming',
      category: 'SPBA',
    },
    slotsElected: '0.08%',
  },
];

const previousEpochName = 'Epoch 93';
const previousEpochData = [
  {
    pool: {
      title: 'Help the USA Cats',
      category: 'CATS',
    },
    slotsElected: '216 slots - 1%',
    performance: '216 of 216 - 100%',
    sharedRewards: '194 ADA of 216 ADA',
  },
  {
    pool: {
      title: 'Cardano Foundation 1',
      category: 'CF1',
    },
    slotsElected: '193 slots - 0.9%',
    performance: '193 of 193 - 100%',
    sharedRewards: '193 ADA of 193 ADA',
  },
  {
    pool: {
      title: 'Blush Pool 1',
      category: 'BLS1',
    },
    slotsElected: '160 slots - 0.78%',
    performance: '158 of 160 - 98%',
    sharedRewards: '79 ADA of 158 ADA',
  },
  {
    pool: {
      title: 'Blush Pool 2',
      category: 'BLS2',
    },
    slotsElected: '101 slots - 0.5%',
    performance: '101 of 101 - 100%',
    sharedRewards: '65 ADA of 101 ADA',
  },
  {
    pool: {
      title: 'Micro Mining',
      category: 'MNG',
    },
    slotsElected: '25 slots - 0.17%',
    performance: '2 of 25 - 8%',
    sharedRewards: '0 ADA of 2 ADA',
  },
  {
    pool: {
      title: 'Saint-Petersburg Academy of Programming',
      category: 'SPBA',
    },
    slotsElected: '11 slots - 0.08%',
    performance: '8 of 11 - 72%',
    sharedRewards: '1 ADA of 8 ADA',
  },
];

export const StakingEpochsStory = () => (
  <StakingEpochs
    currentEpochName={currentEpochName}
    currentEpochData={currentEpochData}
    previousEpochName={previousEpochName}
    previousEpochData={previousEpochData}
    isLoading={false}
  />
);
