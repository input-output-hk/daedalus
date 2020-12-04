// @flow
import React, { Component, Fragment } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import classnames from 'classnames';
import StakePoolsRanking from './StakePoolsRanking';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsTable } from './StakePoolsTable';
import { StakePoolsSearch } from './StakePoolsSearch';
import BackToTopButton from '../../widgets/BackToTopButton';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import Wallet from '../../../domains/Wallet';
import styles from './StakePools.scss';
import { getFilteredStakePoolsList } from './helpers';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  SMASH_SERVER_TYPES,
  SMASH_SERVERS_LIST,
} from '../../../config/stakingConfig';
import smashSettingsIcon from '../../../assets/images/smash-settings-ic.inline.svg';

import type { SmashServerType } from '../../../types/stakingTypes';

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

const SELECTED_INDEX_TABLE = 'selectedIndexTable';
const STAKE_POOLS_DELEGATING_LIST = 'stakePoolsDelegatingList';
const SELECTED_INDEX_LIST = 'selectedIndexList';

type Props = {
  wallets: Array<Wallet>,
  currentLocale: string,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  updateDelegatingStake: Function,
  rankStakePools: Function,
  selectedDelegationWalletId?: ?string,
  stake?: ?number,
  onDelegate: Function,
  isLoading: boolean,
  isRanking: boolean,
  stakePoolsDelegatingList: Array<StakePool>,
  getStakePoolById: Function,
  onSmashSettingsClick: Function,
  smashServerType: SmashServerType,
  smashServerUrl: string,
};

type State = {
  search: string,
  selectedList?: ?string,
  isGridView: boolean,
  isListView: boolean,
  isTableHeaderHovered: boolean,
};

const initialState = {
  search: '',
  selectedList: null,
  isGridView: true,
  isListView: false,
  isTableHeaderHovered: false,
};

@observer
export default class StakePools extends Component<Props, State> {
  loadingSpinner: ?LoadingSpinner;

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = { ...initialState };

  handleSearch = (search: string) => this.setState({ search });

  handleClearSearch = () => this.setState({ search: '' });

  handleGridView = () =>
    this.setState({
      isGridView: true,
      isListView: false,
    });

  handleListView = () =>
    this.setState({
      isGridView: false,
      isListView: true,
    });

  handleSetListActive = (selectedList: string) =>
    this.setState({ selectedList });

  handleTableHeaderMouseEnter = () =>
    this.setState({ isTableHeaderHovered: true });

  handleTableHeaderMouseLeave = () =>
    this.setState({ isTableHeaderHovered: false });

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
      updateDelegatingStake,
      rankStakePools,
      selectedDelegationWalletId,
      stake,
      onOpenExternalLink,
      currentTheme,
      isLoading,
      isRanking,
      stakePoolsDelegatingList,
      getStakePoolById,
      smashServerType,
      smashServerUrl,
      onSmashSettingsClick,
    } = this.props;
    const {
      search,
      selectedList,
      isListView,
      isGridView,
      isTableHeaderHovered,
    } = this.state;

    const filteredStakePoolsList: Array<StakePool> = getFilteredStakePoolsList(
      stakePoolsList,
      search
    );

    const numberOfRankedStakePools: number = stakePoolsList.filter(
      (stakePool) =>
        IS_RANKING_DATA_AVAILABLE && stakePool.nonMyopicMemberRewards
    ).length;

    const listTitleMessage = search.trim().length
      ? messages.listTitleWithSearch
      : messages.listTitle;

    const loadingSpinner = (
      <LoadingSpinner
        big
        ref={(component) => {
          this.loadingSpinner = component;
        }}
      />
    );

    const componentClasses = classnames([
      styles.component,
      isLoading ? styles.isLoading : null,
    ]);

    const smashServer =
      smashServerType === SMASH_SERVER_TYPES.CUSTOM
        ? smashServerUrl
        : SMASH_SERVERS_LIST[smashServerType].name;

    const smashSettings = smashServer && (
      <button onClick={onSmashSettingsClick} className={styles.smashSettings}>
        <span>&nbsp;- {smashServer}</span>
        <SVGInline
          svg={smashSettingsIcon}
          className={styles.smashSettingsIcon}
        />
      </button>
    );

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
              scrollTopToActivate={isListView ? 400 : 340}
              buttonTopPosition={isListView ? 184 : 144}
              isForceHidden={isTableHeaderHovered}
            />
            <StakePoolsRanking
              wallets={wallets}
              currentLocale={currentLocale}
              onOpenExternalLink={onOpenExternalLink}
              updateDelegatingStake={updateDelegatingStake}
              rankStakePools={rankStakePools}
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
              onGridView={this.handleGridView}
              onListView={this.handleListView}
              isListView={isListView}
              isGridView={isGridView}
              smashServer={smashServer}
              isClearTooltipOpeningDownward
            />
            {stakePoolsDelegatingList.length > 0 && (
              <Fragment>
                <h2 className={styles.listTitle}>
                  {intl.formatMessage(messages.delegatingListTitle)}
                  {smashSettings}
                </h2>
                <StakePoolsList
                  listName={STAKE_POOLS_DELEGATING_LIST}
                  stakePoolsList={stakePoolsDelegatingList}
                  onOpenExternalLink={onOpenExternalLink}
                  currentTheme={currentTheme}
                  isListActive={selectedList === STAKE_POOLS_DELEGATING_LIST}
                  setListActive={this.handleSetListActive}
                  containerClassName="StakingWithNavigation_page"
                  onSelect={this.onDelegate}
                  numberOfRankedStakePools={numberOfRankedStakePools}
                  showWithSelectButton
                />
              </Fragment>
            )}
            {isListView && (
              <Fragment>
                <h2>
                  <FormattedMessage
                    {...listTitleMessage}
                    values={{
                      pools: filteredStakePoolsList.length,
                    }}
                  />
                  {smashSettings}
                </h2>
                <StakePoolsTable
                  listName={SELECTED_INDEX_TABLE}
                  currentLocale={currentLocale}
                  stakePoolsList={filteredStakePoolsList}
                  onOpenExternalLink={onOpenExternalLink}
                  currentTheme={currentTheme}
                  isListActive={selectedList === SELECTED_INDEX_TABLE}
                  setListActive={this.handleSetListActive}
                  containerClassName="StakingWithNavigation_page"
                  onSelect={this.onDelegate}
                  numberOfRankedStakePools={numberOfRankedStakePools}
                  showWithSelectButton
                  onTableHeaderMouseEnter={this.handleTableHeaderMouseEnter}
                  onTableHeaderMouseLeave={this.handleTableHeaderMouseLeave}
                />
              </Fragment>
            )}
            {isGridView && (
              <Fragment>
                <h2>
                  <FormattedMessage
                    {...listTitleMessage}
                    values={{
                      pools: filteredStakePoolsList.length,
                    }}
                  />
                  {smashSettings}
                </h2>
                <StakePoolsList
                  showWithSelectButton
                  listName={SELECTED_INDEX_LIST}
                  stakePoolsList={filteredStakePoolsList}
                  onOpenExternalLink={onOpenExternalLink}
                  currentTheme={currentTheme}
                  isListActive={selectedList === SELECTED_INDEX_LIST}
                  setListActive={this.handleSetListActive}
                  containerClassName="StakingWithNavigation_page"
                  onSelect={this.onDelegate}
                  numberOfRankedStakePools={numberOfRankedStakePools}
                />
              </Fragment>
            )}
          </Fragment>
        )}
      </div>
    );
  }
}
