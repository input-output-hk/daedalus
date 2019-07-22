// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import classnames from 'classnames';
import styles from './StakePoolsSearch.scss';
import searchIcon from '../../../assets/images/search.inline.svg';
import closeIcon from '../../../assets/images/close-cross.inline.svg';

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.search.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools search.',
  },
  filterAll: {
    id: 'staking.stakePools.search.filterAll',
    defaultMessage: '!!!All',
    description: '"Filter All" for the Stake Pools search.',
  },
  filterNew: {
    id: 'staking.stakePools.search.filterNew',
    defaultMessage: '!!!New',
    description: '"Filter New" for the Stake Pools search.',
  },
  filterCharity: {
    id: 'staking.stakePools.search.filterCharity',
    defaultMessage: '!!!Charity',
    description: '"FilterChar ity" for the Stake Pools search.',
  },
  delegatingListTitle: {
    id: 'staking.stakePools.search.delegatingListTitle',
    defaultMessage: '!!!Stake pools you are currently delegating to',
    description: '"delegatingListTitlee" for the Stake Pools search.',
  },
  listTitle: {
    id: 'staking.stakePools.search.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools search.',
  },
});

export type Filters = Array<Filter>;
export type Filter = 'all' | 'charity' | 'new';

type Props = {
  filters?: Filters,
  label?: string,
  placeholder?: string,
  isClearTooltipOpeningDownward?: boolean,
  onSearch: Function,
  onClearSearch: Function,
  onFilterChange?: Function,
  search: string,
};

export class StakePoolsSearch extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  searchInput: ?Object = null;

  getFilterItemClassName = (item: string) => {
    const { filters = [] } = this.props;
    if (item === 'all') {
      return classnames({
        [styles.searchFilterActiveItem]: !filters.length,
      });
    }
    return classnames({
      [styles.searchFilterActiveItem]:
        filters.length && filters.indexOf(item) > -1,
    });
  };

  autoSelectOnFocus = () => {
    try {
      const { searchInput } = this;
      if (searchInput) {
        return searchInput.inputElement.current.select();
      }
      return false;
    } catch (error) {
      throw error;
    }
  };

  get hasSearchClearButton() {
    return this.props.search.length > 0;
  }

  get hasFilters() {
    const { filters, onFilterChange } = this.props;
    return !!filters && !!onFilterChange;
  }

  render() {
    const { intl } = this.context;
    const {
      label,
      filters,
      onSearch,
      onClearSearch,
      onFilterChange,
      placeholder,
      search,
      isClearTooltipOpeningDownward,
    } = this.props;

    const filterAll =
      !!filters && !!onFilterChange && onFilterChange.bind(this, 'all');
    const filterNew =
      !!filters && !!onFilterChange && onFilterChange.bind(this, 'new');
    const filterCharity =
      !!filters && !!onFilterChange && onFilterChange.bind(this, 'charity');

    const clearSearchStyles = classnames(styles.clearSearch, {
      [styles.clearSearchSeparator]: !!filters,
    });

    const searchInputStyles = classnames(styles.searchInput, {
      [styles.hasFilters]: !!filters && !!onFilterChange,
    });

    return (
      <div className={styles.component}>
        <div className={styles.container}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            label={label || null}
            className={searchInputStyles}
            onChange={onSearch}
            ref={input => {
              this.searchInput = input;
            }}
            placeholder={
              placeholder || intl.formatMessage(messages.searchInputPlaceholder)
            }
            skin={InputSkin}
            value={search}
            maxLength={150}
            onFocus={this.autoSelectOnFocus}
          />
          {(this.hasSearchClearButton || this.hasFilters) && (
            <div className={styles.inputExtras}>
              {this.hasSearchClearButton && (
                <Tooltip
                  skin={TooltipSkin}
                  tip="Clear"
                  className={clearSearchStyles}
                  isOpeningUpward={!isClearTooltipOpeningDownward}
                >
                  <button onClick={onClearSearch}>
                    <SVGInline svg={closeIcon} />
                  </button>
                </Tooltip>
              )}
              {this.hasFilters && (
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
              )}
            </div>
          )}
        </div>
      </div>
    );
  }
}
