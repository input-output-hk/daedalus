// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsSearch } from './StakePoolsSearch';
import type { StakePool } from '../../../api/staking/types';
import styles from './StakePools.scss';

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

@observer
export default class StakePools extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    search: '',
    filter: 'all',
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  handleFilterChange = (filter: string) => this.setState({ filter });

  handleSearch = (search: string) => this.setState({ search });

  handleSetListActive = (selectedList: string) =>
    this.setState({ selectedList });

  render() {
    const { intl } = this.context;
    const {
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
    } = this.props;
    const {
      search,
      filter,
      flipHorizontal,
      flipVertical,
      selectedList,
    } = this.state;

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
            showWithSelectButton
            listName="stakePoolsDelegatingList"
            flipHorizontal={flipHorizontal}
            flipVertical={flipVertical}
            stakePoolsList={stakePoolsDelegatingList}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            isListActive={selectedList === 'stakePoolsDelegatingList'}
            setListActive={this.handleSetListActive}
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
          showWithSelectButton
          listName="selectedIndexList"
          flipHorizontal={flipHorizontal}
          flipVertical={flipVertical}
          stakePoolsList={stakePoolsList}
          onOpenExternalLink={onOpenExternalLink}
          currentTheme={currentTheme}
          isListActive={selectedList === 'selectedIndexList'}
          setListActive={this.handleSetListActive}
          onSelect={() => {}}
        />
      </div>
    );
  }
}
