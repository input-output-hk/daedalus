// @flow
import React from 'react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { FormattedMessage } from 'react-intl';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { StakePoolsTable } from '../../../source/renderer/app/components/staking/stake-pools/StakePoolsTable';
import { StakePoolsSearch } from '../../../source/renderer/app/components/staking/stake-pools/StakePoolsSearch';

type Props = {
  currentTheme: string,
};

const listTitle = {
  id: 'staking.stakePools.listTitle',
  defaultMessage: '!!!Stake pools ({pools})',
  description: '"listTitle" for the Stake Pools page.',
};

export const StakePoolsTableStory = (props: Props) => (
  <React.Fragment>
    <div
      style={{
        margin: '0 20px 20px',
        display: 'flex',
        flex: 1,
        flexDirection: 'column',
      }}
    >
      <StakePoolsSearch
        search={''}
        onSearch={action('onOpenExternalLink')}
        onClearSearch={action('onOpenExternalLink')}
        onGridView={action('onOpenExternalLink')}
        onListView={action('onOpenExternalLink')}
        isListView
        isGridView={false}
        isClearTooltipOpeningDownward
      />
      <h2
        style={{
          lineHeight: 1.38,
          margin: '20px 0 10px',
          opacity: 0.5,
          paddingLeft: '20px',
          fontFamily: '"NotoSans-Regular, NotoSansCJKjp-Regular", sans-serif',
        }}
      >
        <FormattedMessage
          {...listTitle}
          values={{
            pools: STAKE_POOLS.slice(
              0,
              number('Pools', 300, {
                range: true,
                min: 37,
                max: 300,
                step: 1,
              })
            ).length,
          }}
        />
      </h2>
      <StakePoolsTable
        showWithSelectButton
        listName="selectedIndexList"
        stakePoolsList={STAKE_POOLS.slice(
          0,
          number('Pools', 300, {
            range: true,
            min: 37,
            max: 300,
            step: 1,
          })
        )}
        onOpenExternalLink={action('onOpenExternalLink')}
        currentTheme={props.currentTheme}
        isListActive
        setListActive={action('setListActive')}
        containerClassName="StakingWithNavigation_page"
        onSelect={action('onSelect')}
        numberOfStakePools={
          STAKE_POOLS.slice(
            0,
            number('Pools', 300, {
              range: true,
              min: 37,
              max: 300,
              step: 1,
            })
          ).length
        }
      />
    </div>
  </React.Fragment>
);
