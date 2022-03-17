import React, { useMemo } from 'react';
import { orderBy } from 'lodash';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { FormattedHTMLMessage } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { PoolPopOver } from '../widgets/PoolPopOver';
import { Intl } from '../../../types/i18nTypes';

import styles from './StakePoolsTable.scss';
import StakePool from '../../../domains/StakePool';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import { messages } from './StakePoolsTable.messages';

const ascOrder = 'asc';
const descOrder = 'desc';

export const defaultTableOrdering = {
  ranking: ascOrder,
  ticker: ascOrder,
  saturation: ascOrder,
  cost: ascOrder,
  profitMargin: ascOrder,
  producedBlocks: descOrder,
  nonMyopicMemberRewards: descOrder,
  pledge: ascOrder,
  retiring: ascOrder,
};

interface UseSortedStakePoolListArgs {
  stakePoolList: StakePool[];
  sortBy: string;
  order: 'asc' | 'desc';
}

export const useSortedStakePoolList = ({
  stakePoolList,
  sortBy,
  order,
}: UseSortedStakePoolListArgs) =>
  useMemo(
    () =>
      orderBy(
        stakePoolList.map((stakePool) => {
          let calculatedPledge;
          let calculatedCost;
          let formattedTicker;

          if (sortBy === 'ticker') {
            formattedTicker = stakePool.ticker
              .replace(/[^\w\s]/gi, '')
              .toLowerCase();
          }

          if (sortBy === 'pledge') {
            const formattedPledgeValue = stakePool.pledge.toFixed(2);
            calculatedPledge = Number(
              parseFloat(formattedPledgeValue).toFixed(2)
            );
          }

          if (sortBy === 'cost') {
            const formattedCostValue = stakePool.cost.toFixed(2);
            calculatedCost = Number(parseFloat(formattedCostValue).toFixed(2));
          }

          return {
            ...stakePool,
            calculatedPledge,
            calculatedCost,
            formattedTicker,
          };
        }),
        ['formattedTicker', 'calculatedPledge', 'calculatedCost', sortBy],
        [order, order, order, order]
      ),
    [stakePoolList, order, sortBy]
  );

type UseCreateColumnsArgs = {
  currentTheme: string;
  showWithSelectButton?: boolean;
  onSelect?: (...args: Array<any>) => any;
  containerClassName: string;
  numberOfRankedStakePools: number;
  onOpenExternalLink: (...args: Array<any>) => any;
  intl: Intl;
};

export const useCreateColumns = ({
  numberOfRankedStakePools,
  intl,
  currentTheme,
  onOpenExternalLink,
  onSelect,
  containerClassName,
  showWithSelectButton,
}: UseCreateColumnsArgs) =>
  useMemo(
    () => [
      {
        id: 'ranking',
        Header: (
          <PopOver
            key="ranking"
            placement="bottom"
            content={
              <div className={styles.tooltipWithHtmlContent}>
                <FormattedHTMLMessage {...messages.tableHeaderRankTooltip} />
              </div>
            }
          >
            {intl.formatMessage(messages.tableHeaderRank)}
          </PopOver>
        ),
        accessor: 'ranking',
        Cell: ({ row }) => {
          const { potentialRewards, ranking }: StakePool = row.original;
          const memberRewards = new BigNumber(potentialRewards);

          return (
            <>
              {!memberRewards.isZero() ? (
                ranking
              ) : (
                <>
                  {numberOfRankedStakePools + 1}
                  <span className={styles.asterisk}>*</span>
                </>
              )}
            </>
          );
        },
      },

      {
        id: 'ticker',
        Header: intl.formatMessage(messages.tableHeaderTicker),
        accessor: 'ticker',
        Cell: ({ row }) => {
          const stakePool: StakePool = row.original;
          const color = getColorFromRange(
            stakePool.ranking,
            numberOfRankedStakePools
          );

          return (
            <PoolPopOver
              color={color}
              currentTheme={currentTheme}
              onOpenExternalLink={onOpenExternalLink}
              onSelect={onSelect}
              stakePool={stakePool}
              containerClassName={containerClassName}
              numberOfRankedStakePools={numberOfRankedStakePools}
              showWithSelectButton={showWithSelectButton}
            >
              <span className={styles.ticker} role="presentation">
                {stakePool.ticker}
              </span>
            </PoolPopOver>
          );
        },
      },
      {
        id: 'saturation',
        Header: (
          <PopOver
            key="saturation"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderSaturationTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderSaturation)}
          </PopOver>
        ),
        accessor: 'saturation',
        Cell: ({ row }) => {
          const { saturation }: StakePool = row.original;
          const progressBarContentClassnames = classNames([
            styles.progressBarContent,
            styles[getSaturationColor(saturation)],
          ]);

          return (
            <div className={styles.saturation}>
              <div className={styles.progressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={progressBarContentClassnames}
                    style={{
                      width: `${saturation.toFixed(2)}%`,
                    }}
                  />
                </div>
              </div>
              <div className={styles.saturationLabel}>
                {`${toFixedUserFormat(saturation, 2)}%`}
              </div>
            </div>
          );
        },
      },
      {
        id: 'cost',
        Header: (
          <PopOver
            key="cost"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderCostTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderCost)}
          </PopOver>
        ),
        accessor: 'cost',
        Cell: ({ value }) => {
          const cost = new BigNumber(value);
          const costValue = formattedWalletAmount(cost, false, false);

          return costValue;
        },
      },
      {
        id: 'profitMargin',
        Header: (
          <PopOver
            key="profitMargin"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderMarginTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderMargin)}
          </PopOver>
        ),
        accessor: 'profitMargin',
        Cell: ({ value }) => {
          return `${toFixedUserFormat(value, 2)}%`;
        },
      },
      {
        id: 'producedBlocks',
        Header: (
          <PopOver
            key="producedBlocks"
            placement="bottom"
            content={intl.formatMessage(
              messages.tableHeaderProducedBlocksTooltip
            )}
          >
            {intl.formatMessage(messages.tableHeaderProducedBlocks)}
          </PopOver>
        ),
        accessor: 'producedBlocks',
        Cell: ({ value }) => {
          return toFixedUserFormat(value, 0);
        },
      },
      {
        id: 'nonMyopicMemberRewards',
        Header: (
          <PopOver
            key="nonMyopicMemberRewards"
            placement="bottom"
            content={intl.formatMessage(
              messages.tableHeaderPotentialRewardsTooltip
            )}
          >
            {intl.formatMessage(messages.tableHeaderPotentialRewards)}
          </PopOver>
        ),
        accessor: 'nonMyopicMemberRewards',
        Cell: ({ row }) => {
          const stakePool: StakePool = row.original;
          const memberRewards = new BigNumber(stakePool.potentialRewards);
          const potentialRewards = formattedWalletAmount(memberRewards);
          return potentialRewards;
        },
      },
      {
        id: 'pledge',
        Header: (
          <PopOver
            key="pledge"
            placement="bottom"
            content={intl.formatMessage(messages.tableHeaderPledgeTooltip)}
          >
            {intl.formatMessage(messages.tableHeaderPledge)}
          </PopOver>
        ),
        accessor: 'pledge',
        Cell: ({ row }) => {
          const stakePool: StakePool = row.original;
          const pledge = new BigNumber(stakePool.pledge);
          const pledgeValue = formattedWalletAmount(pledge, false, false);
          return pledgeValue;
        },
      },
      {
        id: 'retiring',
        Header: intl.formatMessage(messages.tableHeaderRetiring),
        accessor: 'retiring',
        Cell: ({ row }) => {
          const stakePool: StakePool = row.original;
          const retirement =
            stakePool.retiring &&
            moment(stakePool.retiring).locale(intl.locale).fromNow(true);

          return (
            <>
              {retirement ? (
                <span className={styles.retiring}>{retirement}</span>
              ) : (
                '-'
              )}
            </>
          );
        },
      },
    ],
    [numberOfRankedStakePools]
  );
