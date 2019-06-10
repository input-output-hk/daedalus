// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './StakePoolsSearch.scss';
import searchIcon from '../../../assets/images/search.inline.svg';

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
  search: string,
  filter: string,
  onSearch: Function,
  onFilterChange: Function,
  registerSearchInput: Function,
};

export class StakePoolsSearch extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getFilterItemClassName = (item: string) =>
    item === this.props.filter && styles.searchFilterActiveItem;

  render() {
    const { intl } = this.context;
    const {
      search,
      onSearch,
      onFilterChange,
      registerSearchInput,
    } = this.props;

    const filterAll = onFilterChange.bind(this, 'all');
    const filterNew = onFilterChange.bind(this, 'new');
    const filterCharity = onFilterChange.bind(this, 'charity');

    return (
      <div className={styles.component}>
        <div className={styles.container}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            className={styles.searchInput}
            onChange={onSearch}
            ref={input => registerSearchInput(input)}
            placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
            skin={InputSkin}
            value={search}
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
      </div>
    );
  }
}
