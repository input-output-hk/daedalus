// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import { debounce, get, map, orderBy } from 'lodash';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import classNames from 'classnames';
import { getRelativePosition } from '../../../utils/domManipulation';
import {
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import styles from './StakePoolsTable.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import TooltipPool from '../widgets/TooltipPool';
import StakePool from '../../../domains/StakePool';

type TableBodyProps = {
  stakePoolsList: Array<StakePool>,
  sortedStakePoolList: StakePool,
  numberOfRankedStakePools: number,
  currentTheme: string,
  onOpenExternalLink: Function,
  showWithSelectButton?: boolean,
  containerClassName: string,
  onSelect?: Function,
  selectedPoolId?: ?number,
  setListActive?: Function,
  isListActive?: boolean,
  listName?: string,
  stakePoolsSortBy: string,
  stakePoolsOrder: string,
};

type TableBodyState = {
  highlightedPoolId: ?number,
  selectedRow: number | null,
  top: number,
  left: number,
};

const initialTableBodyState = {
  highlightedPoolId: null,
  selectedRow: null,
  top: 0,
  left: 0,
};

@observer
export class StakePoolsTableBody extends Component<
  TableBodyProps,
  TableBodyState
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    ...initialTableBodyState,
  };

  constructor(props: TableBodyProps) {
    super(props);
    window.addEventListener('resize', this.handleResize);
  }

  handleResize = () =>
    debounce(this.handleCloseTooltip, 200, { leading: true, trailing: false });

  handleOpenTooltip = (poolId: SyntheticMouseEvent<HTMLElement>) => {
    const {
      isListActive,
      setListActive,
      listName,
      stakePoolsList,
      containerClassName,
      stakePoolsSortBy,
      stakePoolsOrder,
    } = this.props;
    if (poolId.target) {
      poolId.persist();
      const targetElement = poolId.target;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = getRelativePosition(
          targetElement,
          `.${containerClassName}`
        );
        this.setState({ top, left });
        const parentEl = targetElement.parentElement;
        if (parentEl && parentEl.parentElement) {
          const index = get(parentEl.parentElement, 'sectionRowIndex', null);
          this.setState({ selectedRow: index });
        }
      }
    }
    if (isListActive === false && setListActive) setListActive(listName);
    const sortedStakePoolList = orderBy(
      stakePoolsList.map((stakePool) => {
        let calculatedPledge;
        let calculatedCost;
        let formattedTicker;
        if (stakePoolsSortBy === 'ticker') {
          formattedTicker = stakePool.ticker
            .replace(/[^\w\s]/gi, '')
            .toLowerCase();
        }
        if (stakePoolsSortBy === 'pledge') {
          const formattedPledgeValue = stakePool.pledge.toFixed(2);
          calculatedPledge = Number(
            parseFloat(formattedPledgeValue).toFixed(2)
          );
        }
        if (stakePoolsSortBy === 'cost') {
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
      [
        'formattedTicker',
        'calculatedPledge',
        'calculatedCost',
        stakePoolsSortBy,
      ],
      [stakePoolsOrder, stakePoolsOrder, stakePoolsOrder, stakePoolsOrder]
    );

    const targetEl = poolId.currentTarget;
    const { parentElement } = targetEl;
    if (parentElement) {
      const currentTargetChildren = get(
        parentElement.parentElement,
        'sectionRowIndex',
        null
      );
      const highlightedPoolId = sortedStakePoolList[currentTargetChildren]
        ? sortedStakePoolList[currentTargetChildren].id
        : null;
      return this.setState({
        highlightedPoolId,
      });
    }
    return null;
  };

  handleCloseTooltip = (item: SyntheticMouseEvent<HTMLElement>) => {
    const { isListActive, setListActive } = this.props;
    let selectedRow = null;
    if (item) {
      const { target } = item;
      const parent = get(target, 'parentElement', null);
      selectedRow = get(parent, 'sectionRowIndex', null);
    }
    this.setState({
      ...initialTableBodyState,
      selectedRow,
    });
    if (isListActive !== false && setListActive) setListActive(null);
  };

  handleSelect = (stakePoolId: number) => {
    const { onSelect } = this.props;
    const selectedPoolId =
      this.props.selectedPoolId === stakePoolId ? null : stakePoolId;
    if (onSelect) {
      onSelect(selectedPoolId);
    }
  };

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  render() {
    const {
      sortedStakePoolList,
      numberOfRankedStakePools,
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
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

      const isHighlighted = this.getIsHighlighted(stakePool.id);
      const color = getColorFromRange(rank, numberOfRankedStakePools);
      const { top, left, selectedRow } = this.state;

      return (
        <tr
          key={key}
          className={
            selectedRow && selectedRow === key ? styles.selected : null
          }
        >
          <td>
            {!memberRewards.isZero() ? rank : numberOfRankedStakePools + 1}
            {isHighlighted && (
              <TooltipPool
                stakePool={stakePool}
                isVisible
                onClick={this.handleCloseTooltip}
                currentTheme={currentTheme}
                onOpenExternalLink={onOpenExternalLink}
                top={top}
                left={left}
                fromStakePool
                color={color}
                onSelect={() => {
                  this.handleSelect(stakePool.id);
                }}
                showWithSelectButton={showWithSelectButton}
                containerClassName={containerClassName}
                numberOfRankedStakePools={numberOfRankedStakePools}
                isListView
              />
            )}
          </td>
          <td>
            <span
              className={styles.ticker}
              role="presentation"
              onClick={this.handleOpenTooltip}
            >
              {ticker}
            </span>
          </td>
          <td>
            <div className={styles.saturation}>
              <div className={styles.progressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={progressBarContentClassnames}
                    style={{ width: `${parseFloat(saturation).toFixed(2)}%` }}
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
