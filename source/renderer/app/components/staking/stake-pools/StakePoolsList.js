// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { debounce } from 'lodash';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { StakePoolThumbnail } from './StakePoolThumbnail';
import type { StakePool } from '../../../api/staking/types';

import searchIcon from '../../../assets/images/search.inline.svg';
import styles from './StakePoolsList.scss';

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools page.',
  },
  filterAll: {
    id: 'staking.stakePools.filterAll',
    defaultMessage: '!!!All',
    description: '"Filter All" for the Stake Pools page.',
  },
  filterNew: {
    id: 'staking.stakePools.filterNew',
    defaultMessage: '!!!New',
    description: '"Filter New" for the Stake Pools page.',
  },
  filterCharity: {
    id: 'staking.stakePools.filterCharity',
    defaultMessage: '!!!Charity',
    description: '"FilterChar ity" for the Stake Pools page.',
  },
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

@observer
export default class StakePoolsList extends Component<Props, State> {
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

  getFilterItemClassName = (item: string) =>
    item === this.state.filter && styles.searchFilterActiveItem;

  onFilterChange = (filter: string) => this.setState({ filter });

  onSearch = (search: string) => this.setState({ search });

  getIndex = (ranking: number, listLength: number) =>
    ((ranking - 1) * 100) / listLength;

  isSelected = (newSelectedList: string, newSelectedIndex: number) =>
    newSelectedList === this.state.selectedList &&
    newSelectedIndex === this.state.selectedIndex;

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
    event.persist();
    if (event.target instanceof HTMLElement) {
      const targetElement =
        event.target.className === 'StakePool_content'
          ? event.target
          : event.target.parentNode;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = targetElement.getBoundingClientRect();
        const flipHorizontal = left > window.innerWidth - window.innerWidth / 2;
        const flipVertical = top > window.innerHeight - window.innerHeight / 2;
        return this.setState({
          selectedList,
          selectedIndex,
          flipHorizontal,
          flipVertical,
        });
      }
    }
    return false;
  };

  handleClose = () => this.setState({ ...initialState });

  renderDelegatingList = () => {
    const { flipHorizontal, flipVertical } = this.state;
    const {
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
    } = this.props;

    return (
      <div className={styles.stakePoolsDelegatingList}>
        {stakePoolsDelegatingList.map(stakePool => {
          const index = this.getIndex(stakePool.ranking, stakePoolsList.length);
          const isSelected = this.isSelected(
            'selectedIndexDelegatedList',
            stakePool.ranking
          );

          const handleThumbnailClick = (
            event: SyntheticMouseEvent<HTMLElement>
          ) =>
            this.handleClick(
              'selectedIndexDelegatedList',
              event,
              stakePool.ranking
            );
          return (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              isSelected={isSelected}
              onClose={this.handleClose}
              handleClick={handleThumbnailClick}
              onOpenExternalLink={onOpenExternalLink}
              currentTheme={currentTheme}
              flipHorizontal={flipHorizontal}
              flipVertical={flipVertical}
              index={index}
            />
          );
        })}
      </div>
    );
  };

  renderStakePoolThumbnails = () => {
    const { flipHorizontal, flipVertical } = this.state;
    const { stakePoolsList, onOpenExternalLink, currentTheme } = this.props;

    return (
      <div className={styles.stakePoolsList}>
        {stakePoolsList.map(stakePool => {
          const index = this.getIndex(stakePool.ranking, stakePoolsList.length);
          const isSelected = this.isSelected(
            'selectedIndexList',
            stakePool.ranking
          );

          const handleThumbnailClick = (
            event: SyntheticMouseEvent<HTMLElement>
          ) => this.handleClick('selectedIndexList', event, stakePool.ranking);
          return (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              onOpenExternalLink={onOpenExternalLink}
              isSelected={isSelected}
              onClose={this.handleClose}
              handleClick={handleThumbnailClick}
              currentTheme={currentTheme}
              flipHorizontal={flipHorizontal}
              flipVertical={flipVertical}
              index={index}
            />
          );
        })}
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { stakePoolsDelegatingList, stakePoolsList } = this.props;

    const filterAll = this.onFilterChange.bind('all');
    const filterNew = this.onFilterChange.bind('new');
    const filterCharity = this.onFilterChange.bind('charity');

    return (
      <div className={styles.component}>
        <div className={styles.search}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            className={styles.searchInput}
            onChange={this.onSearch}
            ref={input => {
              this.searchInput = input;
            }}
            placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
            skin={InputSkin}
            value={this.state.search}
            maxLength={150}
          />
          <ul className={styles.searchFilter}>
            <li>
              <button
                onClick={filterAll}
                className={this.getFilterItemClassName('all')}
              >
                {intl.formatMessage(messages.filterAll)}
              </button>
            </li>
            <li>
              <button
                onClick={filterNew}
                className={this.getFilterItemClassName('new')}
              >
                {intl.formatMessage(messages.filterNew)}
              </button>
            </li>
            <li>
              <button
                onClick={filterCharity}
                className={this.getFilterItemClassName('charity')}
              >
                {intl.formatMessage(messages.filterCharity)}
              </button>
            </li>
          </ul>
        </div>

        <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>

        {stakePoolsDelegatingList.length && this.renderDelegatingList()}

        <h2>
          <FormattedMessage
            {...messages.listTitle}
            values={{
              pools: stakePoolsList.length,
            }}
          />
        </h2>

        {this.renderStakePoolThumbnails()}
      </div>
    );
  }
}
