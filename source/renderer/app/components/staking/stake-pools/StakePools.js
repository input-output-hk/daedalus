// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import classnames from 'classnames';
import StakePoolsRanking from './StakePoolsRanking';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsSearch } from './StakePoolsSearch';
import BackToTopButton from '../../widgets/BackToTopButton';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import Wallet from '../../../domains/Wallet';
import styles from './StakePools.scss';
import { getFilteredStakePoolsList } from './helpers';
import StakePool from '../../../domains/StakePool';

const messages = defineMessages({
  delegatingListTitle: {
    id: 'staking.stakePools.delegatingListTitle',
    defaultMessage: '!!!Staking pools you are delegating to',
    description: '"delegatingListTitle" for the Stake Pools page.',
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
  loadingStakePoolsMessage: {
    id: 'staking.stakePools.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
});

type Props = {
  wallets: Array<Wallet>,
  currentLocale: string,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  onRank: Function,
  selectedDelegationWalletId?: ?string,
  stake?: ?number,
  onDelegate: Function,
  isLoading: boolean,
  isRanking: boolean,
  stakePoolsDelegatingList: Array<StakePool>,
  getStakePoolById: Function,
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
  loadingSpinner: ?LoadingSpinner;

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
      wallets,
      currentLocale,
      stakePoolsList,
      onRank,
      selectedDelegationWalletId,
      stake,
      onOpenExternalLink,
      currentTheme,
      isLoading,
      isRanking,
      stakePoolsDelegatingList,
      getStakePoolById,
    } = this.props;
    const { search, selectedList } = this.state;

    const filteredStakePoolsList: Array<StakePool> = getFilteredStakePoolsList(
      stakePoolsList,
      search
    );

    const listTitleMessage = search.trim().length
      ? messages.listTitleWithSearch
      : messages.listTitle;

    const loadingSpinner = (
      <LoadingSpinner
        big
        ref={component => {
          this.loadingSpinner = component;
        }}
      />
    );

    const componentClasses = classnames([
      styles.component,
      isLoading ? styles.isLoading : null,
    ]);

    return (
      <div className={componentClasses}>
        {isLoading ? (
          <div className={styles.loadingBlockWrapper}>
            <p>{intl.formatMessage(messages.loadingStakePoolsMessage)}</p>
            {loadingSpinner}
          </div>
        ) : (
          <Fragment>
            <BackToTopButton
              scrollableElementClassName="StakingWithNavigation_page"
              buttonTopPosition={144}
            />

            <StakePoolsRanking
              wallets={wallets}
              currentLocale={currentLocale}
              onOpenExternalLink={onOpenExternalLink}
              onRank={onRank}
              selectedDelegationWalletId={selectedDelegationWalletId}
              stake={stake}
              isLoading={isLoading}
              isRanking={isRanking}
              numberOfStakePools={stakePoolsList.length}
              getStakePoolById={getStakePoolById}
            />
            <StakePoolsSearch
              search={search}
              onSearch={this.handleSearch}
              onClearSearch={this.handleClearSearch}
              isClearTooltipOpeningDownward
            />

            {stakePoolsDelegatingList.length > 0 && (
              <Fragment>
                <h2 className={styles.listTitle}>
                  {intl.formatMessage(messages.delegatingListTitle)}
                </h2>
                <StakePoolsList
                  listName="stakePoolsDelegatingList"
                  stakePoolsList={stakePoolsDelegatingList}
                  onOpenExternalLink={onOpenExternalLink}
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
              currentTheme={currentTheme}
              isListActive={selectedList === 'selectedIndexList'}
              setListActive={this.handleSetListActive}
              containerClassName="StakingWithNavigation_page"
              onSelect={this.onDelegate}
              numberOfStakePools={stakePoolsList.length}
            />
          </Fragment>
        )}
      </div>
    );
  }
}
