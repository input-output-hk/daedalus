// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import classnames from 'classnames';
import styles from './StakePoolsSearch.scss';
import searchIcon from '../../../assets/images/search.inline.svg';

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
  backToTop: {
    id: 'staking.stakePools.search.backToTop',
    defaultMessage: '!!!Back to top',
    description: '"backToTop" for the Stake Pools search.',
  },
});

export type Filters = Array<Filter>;
export type Filter = 'all' | 'charity' | 'new';

type Props = {
  filters: Filters,
  label?: string,
  placeholder?: string,
  scrollableElementSelector: string,
  backToTopScrollThreashold: number,
  onSearch: Function,
  onFilterChange?: Function,
  registerSearchInput: Function,
  search: string,
};

type State = {
  isBackToTopActive: boolean,
};

export class StakePoolsSearch extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isBackToTopActive: false,
  };

  scrollableDomElement: ?HTMLElement = null;

  componentDidMount() {
    this.scrollableDomElement = document.querySelector(
      this.props.scrollableElementSelector
    );
    if (!this.scrollableDomElement) return false;
    return this.scrollableDomElement.addEventListener(
      'scroll',
      this.getIsBackToTopActive
    );
  }

  getFilterItemClassName = (item: string) => {
    if (item === 'all') {
      return classnames({
        [styles.searchFilterActiveItem]: !this.props.filters.length,
      });
    }
    return classnames({
      [styles.searchFilterActiveItem]:
        this.props.filters.length && this.props.filters.indexOf(item) > -1,
    });
  };

  getIsBackToTopActive = () => {
    const { isBackToTopActive } = this.state;
    const { backToTopScrollThreashold } = this.props;
    if (this.scrollableDomElement instanceof HTMLElement) {
      const scrollPosition = this.scrollableDomElement.scrollTop;
      if (scrollPosition > backToTopScrollThreashold && !isBackToTopActive) {
        this.setState({ isBackToTopActive: true });
      } else if (
        scrollPosition <= backToTopScrollThreashold &&
        isBackToTopActive
      ) {
        this.setState({ isBackToTopActive: false });
      }
    }
  };

  backToTop = () => {
    if (this.scrollableDomElement instanceof HTMLElement) {
      this.scrollableDomElement.scrollTop = 0;
    }
  };

  render() {
    const { intl } = this.context;
    const {
      label,
      onSearch,
      onFilterChange,
      placeholder,
      registerSearchInput,
      search,
    } = this.props;
    const { isBackToTopActive } = this.state;

    const filterAll = onFilterChange && onFilterChange.bind(this, 'all');
    const filterNew = onFilterChange && onFilterChange.bind(this, 'new');
    const filterCharity =
      onFilterChange && onFilterChange.bind(this, 'charity');

    const backToTopBtnStyles = classnames(styles.backToTopBtn, {
      [styles.active]: isBackToTopActive,
    });

    return (
      <div className={styles.component}>
        <div className={styles.container}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            label={label || null}
            className={styles.searchInput}
            onChange={onSearch}
            ref={input => registerSearchInput(input)}
            placeholder={
              placeholder || intl.formatMessage(messages.searchInputPlaceholder)
            }
            skin={InputSkin}
            value={search}
            maxLength={150}
          />
          {onFilterChange && (
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
        <button className={backToTopBtnStyles} onClick={this.backToTop}>
          {intl.formatMessage(messages.backToTop)}
        </button>
      </div>
    );
  }
}
