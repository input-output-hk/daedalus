// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsSearch } from './StakePoolsSearch';
import BackToTopButton from '../../widgets/BackToTopButton';
import styles from './StakePools.scss';
import { getFilteredStakePoolsList } from './helpers';
import StakePool from '../../../domains/StakePool';

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
  stakePoolsDelegatingList: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  getPledgeAddressUrl: Function,
  currentTheme: string,
  onDelegate: Function,
};

type State = {
  search: string,
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
    ...initialState,
  };

  handleSearch = (search: string) => this.setState({ search });
  handleClearSearch = () => this.setState({ search: '' });

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
      getPledgeAddressUrl,
      currentTheme,
    } = this.props;
    const { search, selectedList } = this.state;

    const filteredStakePoolsList: Array<StakePool> = getFilteredStakePoolsList(
      stakePoolsList,
      search
    );

    const listTitleMessage = search.trim().length
      ? messages.listTitleWithSearch
      : messages.listTitle;

    return (
      <div className={styles.component}>
        <BackToTopButton
          scrollableElementClassName="StakingWithNavigation_page"
          buttonTopPosition={144}
        />

        <StakePoolsSearch
          search={search}
          onSearch={this.handleSearch}
          onClearSearch={this.handleClearSearch}
          isClearTooltipOpeningDownward
        />

        {stakePoolsDelegatingList.length > 0 && (
          <Fragment>
            <h2>{intl.formatMessage(messages.delegatingListTitle)}</h2>
            <StakePoolsList
              listName="stakePoolsDelegatingList"
              stakePoolsList={stakePoolsDelegatingList}
              onOpenExternalLink={onOpenExternalLink}
              getPledgeAddressUrl={getPledgeAddressUrl}
              currentTheme={currentTheme}
              isListActive={selectedList === 'stakePoolsDelegatingList'}
              setListActive={this.handleSetListActive}
              containerClassName="StakingWithNavigation_page"
              onSelect={this.onDelegate}
              numberOfStakePools={stakePoolsList.length}
              showWithSelectButton
            />
          </Fragment>
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
          getPledgeAddressUrl={getPledgeAddressUrl}
          currentTheme={currentTheme}
          isListActive={selectedList === 'selectedIndexList'}
          setListActive={this.handleSetListActive}
          containerClassName="StakingWithNavigation_page"
          onSelect={this.onDelegate}
          numberOfStakePools={stakePoolsList.length}
        />
      </div>
    );
  }
}
