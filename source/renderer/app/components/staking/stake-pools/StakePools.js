// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { without } from 'lodash';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsSearch } from './StakePoolsSearch';
import type { StakePoolsListType } from '../../../api/staking/types';
import type { Filters, Filter } from './StakePoolsSearch';
import styles from './StakePools.scss';
import { getFilteredStakePoolsList } from './helpers';

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
  listTitleWithSearch: {
    id: 'staking.stakePools.listTitleWithSearch',
    defaultMessage: '!!!Stake pools. Search results: ({pools})',
    description: '"listTitle" for the Stake Pools page.',
  },
});

type Props = {
  stakePoolsDelegatingList: StakePoolsListType,
  stakePoolsList: StakePoolsListType,
  onOpenExternalLink: Function,
  currentTheme: string,
  onDelegate: Function,
};

type State = {
  search: string,
  filters: Filters,
  selectedList?: ?string,
};

const initialState = {
  selectedList: null,
};

@observer
export default class StakePools extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    search: '',
    filters: [],
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  handleFilterChange = (filter: Filter) => {
    const { filters: currentFilters } = this.state;
    let filters = [];
    if (filter === 'all') {
      filters = [];
    } else {
      filters =
        currentFilters.indexOf(filter) > -1
          ? [...without(currentFilters, filter)]
          : [...currentFilters, filter];
    }
    this.setState({ filters });
  };

  handleSearch = (search: string) => this.setState({ search });

  handleSetListActive = (selectedList: string) =>
    this.setState({ selectedList });

  onDelegate = (poolId: string) => {
    const { onDelegate } = this.props;
    onDelegate(poolId);
  };

  render() {
    const { intl } = this.context;
    const {
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
    } = this.props;
    const { search, filters, selectedList } = this.state;

    const filteredStakePoolsList: StakePoolsListType = getFilteredStakePoolsList(
      stakePoolsList,
      search,
      filters
    );

    const listTitleMessage = search.length
      ? messages.listTitleWithSearch
      : messages.listTitle;

    return (
      <div className={styles.component}>
        <StakePoolsSearch
          search={search}
          filters={filters}
          onSearch={this.handleSearch}
          onFilterChange={this.handleFilterChange}
          registerSearchInput={searchInput => {
            this.searchInput = searchInput;
          }}
          scrollableElementSelector=".StakingWithNavigation_page"
          backToTopScrollThreashold={50}
        />

        <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>

        {stakePoolsDelegatingList.length > 0 && (
          <StakePoolsList
            listName="stakePoolsDelegatingList"
            stakePoolsList={stakePoolsDelegatingList}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            isListActive={selectedList === 'stakePoolsDelegatingList'}
            setListActive={this.handleSetListActive}
            containerClassName="StakingWithNavigation_page"
            onSelect={this.onDelegate}
            showWithSelectButton
          />
        )}

        <h2>
          <FormattedMessage
            {...listTitleMessage}
            values={{
              pools: filteredStakePoolsList.length,
            }}
          />
        </h2>

        <StakePoolsList
          showWithSelectButton
          listName="selectedIndexList"
          stakePoolsList={filteredStakePoolsList}
          onOpenExternalLink={onOpenExternalLink}
          currentTheme={currentTheme}
          isListActive={selectedList === 'selectedIndexList'}
          setListActive={this.handleSetListActive}
          containerClassName="StakingWithNavigation_page"
          onSelect={this.onDelegate}
        />
      </div>
    );
  }
}
