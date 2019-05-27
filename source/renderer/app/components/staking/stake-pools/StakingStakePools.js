// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import StakePool from './StakePool';
import type { StakePoolProps } from '../../../api/staking/types';

import searchIcon from '../../../assets/images/search.inline.svg';
import styles from './StakingStakePools.scss';

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools page.',
  },
  delegatingListTitle: {
    id: 'staking.stakePools.delegatingListTitle',
    defaultMessage: '!!!Staking pools your are delegating to',
    description: '"delegatingListTitlee" for the Stake Pools page.',
  },
  listTitle: {
    id: 'staking.stakePools.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools page.',
  },
});

type Props = {
  stakePoolsDelegatingList: Array<StakePoolProps>,
  stakePoolsList: Array<StakePoolProps>,
  onOpenExternalLink: Function,
};

type State = {
  search: string,
  filter: string,
  selectedList?: string,
  selectedIndex?: number,
};

@observer
export default class StakingStakePools extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    search: '',
    filter: 'all',
    selectedList: null,
    selectedIndex: null,
  };

  searchInput: ?HTMLElement = null;

  getFilterItemClassName = (item: string) =>
    item === this.state.filter && styles.searchFilterAtiveItem;

  onFilterChange = (filter: string) => this.setState({ filter });

  onSearch = (search: string) => this.setState({ search });

  stakePoolSearch = ({ id, name, description }: StakePoolProps) => {
    const { search } = this.state;
    let pass = false;
    if (id.match(new RegExp(search, 'i'))) pass = true;
    if (name.match(new RegExp(search, 'i'))) pass = true;
    if (description.match(new RegExp(search, 'i'))) pass = true;
    return pass;
  };

  getRanking = (index: number) =>
    (index * 100) / this.props.stakePoolsList.length;

  isSelected = (newSelectedList, newSelectedIndex) =>
    newSelectedList === this.state.selectedList &&
    newSelectedIndex === this.state.selectedIndex;

  handleClick = (selectedList: string, selectedIndex: number) => {
    if (
      this.state.selectedList === selectedList &&
      this.state.selectedIndex === selectedIndex
    ) {
      return this.setState({ selectedList: null, selectedIndex: null });
    }
    return this.setState({ selectedList, selectedIndex });
  };

  getList = () => {
    const fullList = this.props.stakePoolsList;
    const filtered = fullList
      .filter(this.stakePoolSearch)
      .map<StakePool>(stakePool => (
        <StakePool
          {...stakePool}
          key={stakePool.id}
          ranking={this.getRanking(stakePool.index)}
          onOpenExternalLink={this.props.onOpenExternalLink}
          isSelected={this.isSelected('selectedIndexList', stakePool.index)}
          onClick={index => this.handleClick('selectedIndexList', index)}
        />
      ));
    return filtered;
  };

  render() {
    const { intl } = this.context;
    const { stakePoolsList, stakePoolsDelegatingList } = this.props;
    const { selectedIndexDelegatedList } = this.state;

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
            autoFocus
          />
          <ul className={styles.searchFilter}>
            <li>
              <button
                onClick={() => this.onFilterChange('all')}
                className={this.getFilterItemClassName('all')}
              >
                All
              </button>
            </li>
            <li>
              <button
                onClick={() => this.onFilterChange('new')}
                className={this.getFilterItemClassName('new')}
              >
                New
              </button>
            </li>
            <li>
              <button
                onClick={() => this.onFilterChange('charity')}
                className={this.getFilterItemClassName('charity')}
              >
                Charity
              </button>
            </li>
          </ul>
        </div>

        <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>

        <div className={styles.stakePoolsDelegatingList}>
          {stakePoolsDelegatingList.map(stakePool => (
            <StakePool
              {...stakePool}
              key={stakePool.id}
              ranking={this.getRanking(stakePool.index)}
              isSelected={this.isSelected(
                'selectedIndexDelegatedList',
                stakePool.index
              )}
              onClick={index =>
                this.handleClick('selectedIndexDelegatedList', index)
              }
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

        <div className={styles.stakePoolsList}>{this.getList()}</div>
      </div>
    );
  }
}
