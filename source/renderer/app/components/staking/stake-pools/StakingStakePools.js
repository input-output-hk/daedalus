// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';

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
  stakePoolsDelegatingList: Array<any>,
  stakePoolsList: Array<any>,
};

type State = {
  search: string,
  filter: string,
};

@observer
export default class StakingStakePools extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    search: '',
    filter: 'all',
  };

  getFilterItemClassName = (item: string) =>
    item === this.state.filter && styles.searchFilterAtiveItem;

  onFilterChange = (filter: string) => this.setState({ filter });

  onSearch = () => {};

  render() {
    const { intl } = this.context;
    const { stakePoolsDelegatingList, stakePoolsList } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.search}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            className={styles.searchInput}
            onKeyPress={this.onSearch}
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
          <pre>{JSON.stringify(stakePoolsDelegatingList)}</pre>
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
          <pre>{JSON.stringify(stakePoolsList)}</pre>
        </div>
      </div>
    );
  }
}
