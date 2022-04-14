import React, { useMemo } from 'react';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { FormattedHTMLMessage } from 'react-intl';
import { Column } from 'react-table';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { PoolPopOver } from '../../widgets/PoolPopOver';
import { Intl } from '../../../../types/i18nTypes';
import styles from '../StakePoolsTable.scss';
import StakePool from '../../../../domains/StakePool';
import {
  getColorFromRange,
  getSaturationColor,
} from '../../../../utils/colors';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../../utils/formatters';
import { messages } from '../StakePoolsTable.messages';
import { StakePoolSortableProps } from './types';

type UseCreateColumnsArgs = {
  currentTheme: string;
  showWithSelectButton?: boolean;
  onSelect?: (poolId: string) => void;
  containerClassName: string;
  numberOfRankedStakePools: number;
  onOpenExternalLink: (url: string) => void;
  intl: Intl;
};

type StakePoolColumn = Column<StakePool> & {
  id: StakePoolSortableProps;
  accessor: StakePoolSortableProps;
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
  useMemo<StakePoolColumn[]>(
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
          const { potentialRewards, ranking } = row.original;
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
          const stakePool = row.original;
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
          const { saturation } = row.original;
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
          const stakePool = row.original;
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
          const stakePool = row.original;
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
          const stakePool = row.original;
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
