// @flow
import React, { Component } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { get, map } from 'lodash';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import classNames from 'classnames';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import { PoolPopOver } from '../widgets/PoolPopOver';
import styles from './StakePoolsTable.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import pledgeNotMetIcon from '../../../assets/images/red-warning.inline.svg';

const messages = defineMessages({
  pledgeNotMetPopOver: {
    id: 'staking.stakePools.tooltip.pledgeNotMet.popover',
    defaultMessage:
      '!!!This pool has not met its pledge requirements. This means that the pool will not produce blocks or generate rewards until the pledge is met.',
    description:
      'Table body "pledgeNotMetPopOver" label on stake pools list view page',
  },
});

type TableBodyProps = {
  sortedStakePoolList: StakePool,
  numberOfRankedStakePools: number,
  currentTheme: string,
  onOpenExternalLink: Function,
  showWithSelectButton?: boolean,
  containerClassName: string,
  onSelect?: (poolId: string) => void,
  selectedPoolId?: ?number,
};

@observer
export class StakePoolsTableBody extends Component<TableBodyProps> {
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
      const pledgeNotMet = get(stakePool, 'pledgeNotMet', false);
      const potentialRewards = new BigNumber(
        get(stakePool, 'potentialRewards', '')
      );
      const formattedPotentialRewards = !potentialRewards.isZero()
        ? formattedWalletAmount(potentialRewards)
        : '?';
      const retirement =
        retiring && moment(retiring).locale(intl.locale).fromNow(true);
      const costValue = formattedWalletAmount(cost, false, false);
      const progressBarContentClassnames = classNames([
        styles.progressBarContent,
        styles[getSaturationColor(saturation)],
      ]);

      const color = getColorFromRange(
        rank,
        numberOfRankedStakePools,
        pledgeNotMet
      );

      let rankValue = rank;
      let pledgeValue = formattedWalletAmount(pledge, false, false);
      if (pledgeNotMet) {
        rankValue = '–';
        pledgeValue = (
          <PopOver
            content={intl.formatMessage(messages.pledgeNotMetPopOver)}
            className={styles.pledgeNotMetPopOver}
          >
            {formattedWalletAmount(pledge, false, false)}
            <SVGInline svg={pledgeNotMetIcon} />
          </PopOver>
        );
      }

      return (
        <tr key={key}>
          <td>{rankValue}</td>
          <td>
            <PoolPopOver
              color={color}
              currentTheme={currentTheme}
              onOpenExternalLink={onOpenExternalLink}
              onSelect={onSelect}
              isSelected={selectedPoolId === stakePool.id}
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
              <div className={styles.saturationLabel}>
                {`${toFixedUserFormat(saturation, 2)}%`}
              </div>
              <div className={styles.progressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={progressBarContentClassnames}
                    style={{ width: `${parseFloat(saturation).toFixed(2)}%` }}
                  />
                </div>
              </div>
            </div>
          </td>
          <td>{costValue}</td>
          <td>{`${toFixedUserFormat(margin, 2)}%`}</td>
          <td>{toFixedUserFormat(producedBlocks, 0)}</td>
          <td>{formattedPotentialRewards}</td>
          <td className={pledgeNotMet ? styles.pledgeNotMet : null}>
            {pledgeValue}
          </td>
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
