// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce } from 'lodash';
import styles from './StakePoolsList.scss';
import type { StakePool } from '../../../api/staking/types';
import { StakePoolThumbnail } from './StakePoolThumbnail';
import { rangeMap } from '../../../utils/rangeMap';

type Props = {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  /**
   *
   * If the parent component has more than one <StakePoolsList />
   * these 3 props need to be passed, as it's the parent who will control
   * which list is active and prevent multiple Tooltips to be displayed
   *
   */
  listName?: string,
  isListActive?: boolean,
  setListActive?: Function,
};

type State = {
  selectedIndex?: ?number,
  tooltipPosition: string,
  tooltipOffset: number,
};

const initialState = {
  selectedIndex: null,
  tooltipPosition: 'right',
  tooltipOffset: 0,
};

const TOOLTIP_DELTA = 20;
const TOOLTIP_WIDTH = 240;
const TOOLTIP_MAX_HEIGHT = 337;

@observer
export class StakePoolsList extends Component<Props, State> {
  static defaultProps = {
    isListActive: true,
  };

  constructor(props: Props) {
    super(props);
    window.addEventListener(
      'resize',
      debounce(this.handleClose, 200, { leading: true, trailing: false })
    );
  }

  state = {
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  getIndex = (ranking: number) =>
    rangeMap(ranking, 1, this.props.stakePoolsList.length, 0, 99);

  getIsSelected = (index: number) => index === this.state.selectedIndex;

  handleClick = (
    event: SyntheticMouseEvent<HTMLElement>,
    selectedIndex: number
  ) => {
    const { isListActive, setListActive, listName } = this.props;
    if (isListActive === false && setListActive) setListActive(listName);
    if (this.state.selectedIndex === selectedIndex) {
      return this.handleClose();
    }
    const { tooltipPosition, tooltipOffset } = this.calculateTooltipPosition(
      event
    );
    if (!tooltipPosition || !tooltipOffset) return false;
    return this.setState({
      selectedIndex,
      tooltipPosition,
      tooltipOffset,
    });
  };

  calculateTooltipPosition = (event: SyntheticMouseEvent<HTMLElement>) => {
    event.persist();
    if (event.target instanceof HTMLElement) {
      const targetElement =
        event.target.className === 'StakePool_content'
          ? event.target
          : event.target.parentNode;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = targetElement.getBoundingClientRect();

        let tooltipPosition = 'right';
        if (top <= TOOLTIP_DELTA) tooltipPosition = 'bottom';
        else if (top >= window.innerHeight - TOOLTIP_DELTA)
          tooltipPosition = 'top';
        else if (left > window.innerWidth - window.innerWidth / 2)
          tooltipPosition = 'left';

        const tooltipOffset = 0;

        return {
          tooltipPosition,
          tooltipOffset,
        };

        // const tooltipPosition = left > window.innerWidth - window.innerWidth / 2;
        // const tooltipOffset = top > window.innerHeight - window.innerHeight / 2;
      }
    }
  };

  handleClose = () => this.setState({ ...initialState });

  render() {
    const {
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      isListActive,
    } = this.props;

    const { tooltipPosition, tooltipOffset } = this.state;

    return (
      <div className={styles.component}>
        {stakePoolsList.map(stakePool => {
          const index = this.getIndex(stakePool.ranking);
          const isSelected =
            this.getIsSelected(stakePool.ranking) && isListActive !== false;
          return (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              onOpenExternalLink={onOpenExternalLink}
              isSelected={isSelected}
              onClose={this.handleClose}
              onClick={this.handleClick}
              currentTheme={currentTheme}
              tooltipPosition={tooltipPosition}
              tooltipOffset={tooltipOffset}
              index={index}
            />
          );
        })}
      </div>
    );
  }
}
