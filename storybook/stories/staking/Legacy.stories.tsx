import React from 'react';
import { storiesOf } from '@storybook/react';
import { observable } from 'mobx';
import StoryDecorator from '../_support/StoryDecorator';
import StakingChart from '../../../source/renderer/app/components/staking/legacy/StakingChart';
import StakingChartTooltip from '../../../source/renderer/app/components/staking/legacy/StakingChartTooltip';

const generateRandomSlots = (count: number) => {
  const slots = [];

  for (let i = 0; i < count; i += 1) {
    const numberOfTransactions =
      i < count / 2 ? Math.floor(Math.random() * 50) : 0;
    slots.push({
      numberOfTransactions,
      slot: slots.length + 1,
      shares: 'CC',
      openings: 'BB',
      commitments: 'AA',
      mpcPhase: 'Shares',
      hash: 'ad9f37d14e189f5d792aaf524a6e0a13cdc5ba13b540f231638444687526231e',
      time: new Date(),
    });
  }

  return slots;
};

storiesOf('StakingChart', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Tooltip only', () => (
    <div
      style={{
        padding: '50px',
      }}
    >
      <StakingChartTooltip
        slot={2848104}
        shares="CC"
        openings="BB"
        commitments="AA"
        mpcPhase="Shares"
        hash="ad9f37d14e189f5d792aaf524a6e0a13cdc5ba13b540f231638444687526231e"
        numberOfTransactions={50}
        time={new Date()}
      />
    </div>
  ))
  .add('Chart with Tooltips', () => {
    const options = observable({
      data: generateRandomSlots(30),
      ticks: [0, 10, 20, 30, 40, 50],
      activeIndex: null,
    });
    return <StakingChart width={500} height={150} options={options} />;
  });
