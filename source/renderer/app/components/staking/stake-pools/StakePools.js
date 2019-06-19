// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { debounce } from 'lodash';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsSearch } from './StakePoolsSearch';
import type { StakePool } from '../../../api/staking/types';
import styles from './StakePools.scss';
import { rangeMap } from '../../../utils/rangeMap';

const messages = defineMessages({
  delegatingListTitle: {
    id: 'staking.stakePools.delegatingListTitle',
    defaultMessage: '!!!Stake pools you are currently delegating to',
    description: '"delegatingListTitlee" for the Stake Pools page.',
  },
  listTitle: {
    id: 'staking.stakePools.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools page.',
  },
});

type Props = {
  stakePoolsDelegatingList: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
};

type State = {
  search: string,
  filter: string,
  selectedList?: ?string,
  selectedIndex?: ?number,
  flipHorizontal: boolean,
  flipVertical: boolean,
};

const initialState = {
  selectedList: null,
  selectedIndex: null,
  flipHorizontal: false,
  flipVertical: false,
};

const TOOLTIP_DELTA = 20;

@observer
export default class StakePools extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    window.addEventListener(
      'resize',
      debounce(this.handleClose, 200, { leading: true, trailing: false })
    );
  }

  state = {
    search: '',
    filter: 'all',
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  handleFilterChange = (filter: string) => this.setState({ filter });

  handleSearch = (search: string) => this.setState({ search });

  getIndex = (ranking: number) =>
    rangeMap(ranking, 1, this.props.stakePoolsList.length, 0, 99);

  getIsSelected = (list: string, index: number) =>
    list === this.state.selectedList && index === this.state.selectedIndex;

  handleClick = (
    selectedList: string,
    event: SyntheticMouseEvent<HTMLElement>,
    selectedIndex: number
  ) => {
    if (
      this.state.selectedList === selectedList &&
      this.state.selectedIndex === selectedIndex
    ) {
      return this.handleClose();
    }
    const { positionX, positionY } = this.getTooltipPosition(event);

    if (!positionY || !positionX) return false;

    return this.setState({
      selectedList,
      selectedIndex,
      positionX,
      positionY,
    });
  };

  getTooltipPosition = (event: SyntheticMouseEvent<HTMLElement>) => {
    event.persist();
    if (event.target instanceof HTMLElement) {
      const targetElement =
        event.target.className === 'StakePool_content'
          ? event.target
          : event.target.parentNode;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = targetElement.getBoundingClientRect();
        const C = left;
        const D = window.innerWidth - left;
        let positionX;
        if (C <= TOOLTIP_DELTA) positionX = 'left';
        else if (left > window.innerWidth - window.innerWidth / 2)
          positionX = 'rightMiddle';
        else if (D <= TOOLTIP_DELTA) positionX = 'right';
        else positionX = 'leftMiddle';
        const positionY =
          top > window.innerHeight - window.innerHeight / 2 ? 'bottom' : 'top';
        return {
          positionX,
          positionY,
        };
      }
    }
    return false;
  };

  handleClose = () => this.setState({ ...initialState });

  render() {
    const { intl } = this.context;
    const {
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
    } = this.props;
    const { search, filter, positionX, positionY } = this.state;

    return (
      <div className={styles.component}>
        <StakePoolsSearch
          search={search}
          filter={filter}
          onSearch={this.handleSearch}
          onFilterChange={this.handleFilterChange}
          registerSearchInput={searchInput => {
            this.searchInput = searchInput;
          }}
        />

        <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>

        {stakePoolsDelegatingList.length && (
          <StakePoolsList
            listName="stakePoolsDelegatingList"
            positionX={positionX}
            positionY={positionY}
            stakePoolsList={stakePoolsDelegatingList}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            getIsSelected={this.getIsSelected}
            onClose={this.handleClose}
            onClick={this.handleClick}
            getIndex={this.getIndex}
          />
        )}

        <h2>
          <FormattedMessage
            {...messages.listTitle}
            values={{
              pools: stakePoolsList.length,
            }}
          />
        </h2>

        <StakePoolsList
          listName="selectedIndexList"
          positionX={positionX}
          positionY={positionY}
          stakePoolsList={stakePoolsList}
          onOpenExternalLink={onOpenExternalLink}
          currentTheme={currentTheme}
          getIsSelected={this.getIsSelected}
          onClose={this.handleClose}
          onClick={this.handleClick}
          getIndex={this.getIndex}
        />
      </div>
    );
  }
}
