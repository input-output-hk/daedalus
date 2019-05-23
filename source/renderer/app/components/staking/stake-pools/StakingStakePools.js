// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
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

  getTip = (stakePool: StakePoolProps) => (
    <pre>{JSON.stringify(stakePool, null, 2)}</pre>
  );

  getRanking = (index: number) =>
    (index * 100) / this.props.stakePoolsList.length;

  getList = () => {
    const fullList = this.props.stakePoolsList;
    const filtered = fullList
      // .filter(this.stakePoolSearch)
      .map<Tooltip>(stakePool => (
        <Tooltip
          key={stakePool.id}
          skin={TooltipSkin}
          tip={this.getTip(stakePool)}
        >
          <StakePool
            {...stakePool}
            ranking={this.getRanking(stakePool.index)}
          />
        </Tooltip>
      ));
    return filtered;
  };

  render() {
    const { intl } = this.context;
    const { stakePoolsList, stakePoolsDelegatingList } = this.props;

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
            <Tooltip
              key={stakePool.id}
              skin={TooltipSkin}
              tip={this.getTip(stakePool)}
            >
              <StakePool
                {...stakePool}
                ranking={this.getRanking(stakePool.index)}
              />
            </Tooltip>
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
