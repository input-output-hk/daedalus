import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import { get, map } from 'lodash';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import classNames from 'classnames';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import { PoolPopOver } from '../widgets/PoolPopOver';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsTable.scss' or its... Remove this comment to see the full error message
import styles from './StakePoolsTable.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';

type TableBodyProps = {
  sortedStakePoolList: StakePool;
  numberOfRankedStakePools: number;
  currentTheme: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  showWithSelectButton?: boolean;
  containerClassName: string;
  onSelect?: (poolId: string) => void;
  selectedPoolId?: number | null | undefined;
};

@observer
class StakePoolsTableBody extends Component<TableBodyProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      sortedStakePoolList,
      numberOfRankedStakePools,
      currentTheme,
      onSelect,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
      selectedPoolId,
    } = this.props;
    const { intl } = this.context;
    return map(sortedStakePoolList, (stakePool, key) => {
      const rank = get(stakePool, 'ranking', '');
      const ticker = get(stakePool, 'ticker', '');
      const saturation = get(stakePool, 'saturation', '');
      const cost = new BigNumber(get(stakePool, 'cost', ''));
      const margin = get(stakePool, 'profitMargin', '');
      const producedBlocks = get(stakePool, 'producedBlocks', '');
      const pledge = new BigNumber(get(stakePool, 'pledge', ''));
      const retiring = get(stakePool, 'retiring', '');
      const memberRewards = new BigNumber(
        get(stakePool, 'potentialRewards', '')
      );
      const potentialRewards = formattedWalletAmount(memberRewards);
      const retirement =
        retiring && moment(retiring).locale(intl.locale).fromNow(true);
      const pledgeValue = formattedWalletAmount(pledge, false, false);
      const costValue = formattedWalletAmount(cost, false, false);
      const progressBarContentClassnames = classNames([
        styles.progressBarContent,
        styles[getSaturationColor(saturation)],
      ]);
      const color = getColorFromRange(rank, numberOfRankedStakePools);
      return (
        <tr key={key}>
          <td>
            {!memberRewards.isZero() ? (
              rank
            ) : (
              <>
                {numberOfRankedStakePools + 1}
                <span className={styles.asterisk}>*</span>
              </>
            )}
          </td>
          <td>
            <PoolPopOver
              color={color}
              currentTheme={currentTheme}
              onOpenExternalLink={onOpenExternalLink}
              onSelect={onSelect}
              // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'string | num... Remove this comment to see the full error message
              isSelected={selectedPoolId === stakePool.id}
              // @ts-ignore ts-migrate(2322) FIXME: Type 'string | number | boolean | BigNumber | Date... Remove this comment to see the full error message
              stakePool={stakePool}
              containerClassName={containerClassName}
              numberOfRankedStakePools={numberOfRankedStakePools}
              showWithSelectButton={showWithSelectButton}
            >
              <span className={styles.ticker} role="presentation">
                {ticker}
              </span>
            </PoolPopOver>
          </td>
          <td>
            <div className={styles.saturation}>
              <div className={styles.progressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={progressBarContentClassnames}
                    style={{
                      width: `${parseFloat(saturation).toFixed(2)}%`,
                    }}
                  />
                </div>
              </div>
              <div className={styles.saturationLabel}>
                {`${toFixedUserFormat(saturation, 2)}%`}
              </div>
            </div>
          </td>
          <td>{costValue}</td>
          <td>{`${toFixedUserFormat(margin, 2)}%`}</td>
          <td>{toFixedUserFormat(producedBlocks, 0)}</td>
          <td>{potentialRewards}</td>
          <td>{pledgeValue}</td>
          <td>
            {retirement ? (
              <span className={styles.retiring}>{retirement}</span>
            ) : (
              '-'
            )}
          </td>
        </tr>
      );
    });
  }
}

export { StakePoolsTableBody };
