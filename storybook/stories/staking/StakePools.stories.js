// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import stakingDecorator from './_utils/stakingDecorator';

import { StakePoolsTileViewStory } from './StakePoolsTileView.stories';
import { StakePoolsListViewStory } from './StakePools-ListView.stories';

const activeItem = 'stake-pools';

storiesOf('Decentralization | Stake Pools', module)
  .addDecorator(stakingDecorator(activeItem))
  // ====== Stories ======

  .add('Tile view', StakePoolsTileViewStory)
  .add('List view', StakePoolsListViewStory)
  .add('Loading', (props) => <StakePoolsTileViewStory {...props} isLoading />);
