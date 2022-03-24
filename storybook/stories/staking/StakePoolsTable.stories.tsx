import React from 'react';
import { number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { FormattedMessage } from 'react-intl';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { StakePoolsTable } from '../../../source/renderer/app/components/staking/stake-pools/StakePoolsTable';
import { StakePoolsSearch } from '../../../source/renderer/app/components/staking/stake-pools/StakePoolsSearch';

const listTitle = {
  id: 'staking.stakePools.listTitle',
  defaultMessage: '!!!Stake pools ({pools})',
  description: '"listTitle" for the Stake Pools page.',
};
type Props = {
  currentTheme: string;
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
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
        listName="selectedIndexList"
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        stakePoolsList={STAKE_POOLS.slice(
          0,
          number('Pools', 300, {
            range: true,
            min: 37,
            max: 300,
            step: 1,
          })
        )}
        currentLocale="en-US"
        currentTheme={props.currentTheme}
        onOpenExternalLink={action('onOpenExternalLink')}
        containerClassName="StakingWithNavigation_page"
        numberOfRankedStakePools={
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
        onTableHeaderMouseEnter={() => {}}
        onTableHeaderMouseLeave={() => {}}
      />
    </div>
  </React.Fragment>
);
