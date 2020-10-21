// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce, get, map, orderBy } from 'lodash';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import classNames from 'classnames';
import { getRelativePosition} from '../../../utils/domManipulation';
import { bigNumbersToFormattedNumbers, formattedLovelaceToAmount, shortNumber } from '../../../utils/formatters';
import styles from './StakePoolsTable.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import TooltipPool from '../widgets/TooltipPool';
import StakePool from '../../../domains/StakePool';

type TableBodyProps = {
  stakePoolsList: Array<StakePool>,
  sortedStakePoolList: StakePool,
  ada: string,
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
export class StakePoolsTableBody extends Component<TableBodyProps, TableBodyState> {
  state = {
    ...initialTableBodyState,
  };

  constructor(props: TableBodyProps) {
    super(props);
    window.addEventListener('resize', this.handleResize);
  }

  handleResize = () =>
    debounce(this.handleCloseTooltip, 200, {leading: true, trailing: false});

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
        const {top, left} = getRelativePosition(
          targetElement,
          `.${containerClassName}`
        );
        this.setState({top, left});
        const parentEl = targetElement.parentElement;
        if (parentEl && parentEl.parentElement) {
          const index = get(parentEl.parentElement, 'sectionRowIndex', null);
          this.setState({selectedRow: index});
        }
      }
    }
    if (isListActive === false && setListActive) setListActive(listName);
    const sortedStakePoolList = orderBy(
      stakePoolsList,
      stakePoolsSortBy,
      stakePoolsOrder
    );
    const targetEl = poolId.currentTarget;
    const {parentElement} = targetEl;
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
    const {isListActive, setListActive} = this.props;
    let selectedRow = null;
    if (item) {
      const {target} = item;
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
    const {onSelect} = this.props;
    const selectedPoolId =
      this.props.selectedPoolId === stakePoolId ? null : stakePoolId;
    if (onSelect) {
      onSelect(selectedPoolId);
    }
  };

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  render() {
    const {ada, sortedStakePoolList, numberOfRankedStakePools, currentTheme, onOpenExternalLink, showWithSelectButton, containerClassName} = this.props;
    return (
      map(sortedStakePoolList, (stakePool, key) => {
        const rank = get(stakePool, 'ranking', '');
        const ticker = get(stakePool, 'ticker', '');
        const saturation = get(stakePool, 'saturation', '');
        const cost = new BigNumber(get(stakePool, 'cost', ''));
        const margin = get(stakePool, 'profitMargin', '');
        const producedBlocks = get(stakePool, 'producedBlocks', '');
        const pledge = new BigNumber(get(stakePool, 'pledge', ''));
        const retiring = get(stakePool, 'retiring', '');
        const memberRewards = get(
          stakePool,
          'nonMyopicMemberRewards',
          ''
        );
        const potentialRewards = memberRewards
          ? `${shortNumber(
            formattedLovelaceToAmount(memberRewards)
          )} ${ada}`
          : '-';
        const isOversaturated = saturation / 100 >= 1;
        const saturationValue =
          isOversaturated || !saturation
            ? parseInt(saturation, 10)
            : parseInt(saturation, 10);
        const calculatedDateRange = moment(retiring).diff(
          moment(),
          'days'
        );

        const pledgeValue = bigNumbersToFormattedNumbers(
          pledge,
          true
        );
        const pledgeCalculatedValue = Number(pledgeValue)
          ? Number(pledgeValue).toFixed(2)
          : pledgeValue;
        const costValue = bigNumbersToFormattedNumbers(cost);

        const saturationBarClassnames = classNames([
          styles.progress,
          styles[getSaturationColor(saturation)],
        ]);

        const isHighlighted = this.getIsHighlighted(stakePool.id);
        const color = getColorFromRange(
          rank,
          numberOfRankedStakePools
        );
        const {top, left, selectedRow} = this.state;

        return (
          <tr
            key={key}
            className={
              selectedRow && selectedRow === key
                ? styles.selected
                : null
            }
          >
            <td>
              {memberRewards ? rank : '-'}
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
                  onSelect={this.handleSelect}
                  showWithSelectButton={showWithSelectButton}
                  containerClassName={containerClassName}
                  numberOfRankedStakePools={
                    numberOfRankedStakePools
                  }
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
              <div className={styles.currentEpochProgressBar}>
                <div className={styles.progressBarContainer}>
                  <div
                    className={saturationBarClassnames}
                    style={{width: `${saturationValue}%`}}
                  />
                  <div className={styles.progressLabel}>
                    {saturationValue}%
                  </div>
                </div>
              </div>
            </td>
            <td>{Number(costValue).toFixed(2)}</td>
            <td>{margin}%</td>
            <td>{producedBlocks}</td>
            <td>{potentialRewards}</td>
            <td>{pledgeCalculatedValue}</td>
            <td>
              {retiring && calculatedDateRange ? (
                <span className={styles.retiring}>
                              {calculatedDateRange === 1
                                ? `${calculatedDateRange} day`
                                : `${calculatedDateRange} days`}
                            </span>
              ) : (
                <span>-</span>
              )}
            </td>
          </tr>
        );
      }));
  }
}
