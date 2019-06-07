// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { debounce } from 'lodash';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import StakePoolThumbnail from './StakePoolThumbnail';
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
    item === this.state.filter && styles.searchFilterAtiveItem;

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

  render() {
    const { intl } = this.context;
    const {
      stakePoolsList,
      stakePoolsDelegatingList,
      onOpenExternalLink,
      currentTheme,
    } = this.props;

    const { flipHorizontal, flipVertical } = this.state;

    return (
      <div className={styles.component}>
        <div className={styles.search}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            className={styles.searchInput}
            onChange={this.onSearch}
            ref={input => {
              this.searchInput = input;
            }}
            placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
            skin={InputSkin}
            value={this.state.search}
            maxLength={150}
            autoFocus
          />
          <ul className={styles.searchFilter}>
            <li>
              <button
                onClick={() => this.onFilterChange('all')}
                className={this.getFilterItemClassName('all')}
              >
                {intl.formatMessage(messages.filterAll)}
              </button>
            </li>
            <li>
              <button
                onClick={() => this.onFilterChange('new')}
                className={this.getFilterItemClassName('new')}
              >
                {intl.formatMessage(messages.filterNew)}
              </button>
            </li>
            <li>
              <button
                onClick={() => this.onFilterChange('charity')}
                className={this.getFilterItemClassName('charity')}
              >
                {intl.formatMessage(messages.filterCharity)}
              </button>
            </li>
          </ul>
        </div>

        <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>

        <div className={styles.stakePoolsDelegatingList}>
          {stakePoolsDelegatingList.map(stakePool => (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              isSelected={this.isSelected(
                'selectedIndexDelegatedList',
                stakePool.ranking
              )}
              onClose={this.handleClose}
              onClick={(...params) =>
                this.handleClick('selectedIndexDelegatedList', ...params)
              }
              onOpenExternalLink={onOpenExternalLink}
              currentTheme={currentTheme}
              flipHorizontal={flipHorizontal}
              flipVertical={flipVertical}
              index={this.getIndex(stakePool.ranking, stakePoolsList.length)}
            />
          ))}
        </div>

        <h2>
          <FormattedMessage
            {...messages.listTitle}
            values={{
              pools: stakePoolsList.length,
            }}
          />
        </h2>

        <div className={styles.stakePoolsList}>
          {stakePoolsList.map(stakePool => (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              onOpenExternalLink={onOpenExternalLink}
              isSelected={this.isSelected(
                'selectedIndexList',
                stakePool.ranking
              )}
              onClose={this.handleClose}
              onClick={(...params) =>
                this.handleClick('selectedIndexList', ...params)
              }
              currentTheme={currentTheme}
              flipHorizontal={flipHorizontal}
              flipVertical={flipVertical}
              index={this.getIndex(stakePool.ranking, stakePoolsList.length)}
            />
          ))}
        </div>
      </div>
    );
  }
}
