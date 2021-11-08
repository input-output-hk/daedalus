// @flow
import React, { Component, Fragment } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';
import StakePoolsRanking from './StakePoolsRanking';
import { StakePoolsList } from './StakePoolsList';
import { StakePoolsTable } from './StakePoolsTable';
import { StakePoolsSearch } from './StakePoolsSearch';
import BackToTopButton from '../../widgets/BackToTopButton';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import Wallet from '../../../domains/Wallet';
import styles from './StakePools.scss';
import { getFilteredStakePoolsList } from './helpers';
import { formattedNumber } from '../../../utils/formatters';
import {
  getNumberOfFilterDimensionsApplied,
  getSmashServerNameFromUrl,
} from '../../../utils/staking';
import type { StakePoolFilterOptionsType } from '../../../stores/StakingStore';
import { defaultStakePoolFilterOptions } from '../../../stores/StakingStore';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  SMASH_SERVER_TYPES,
} from '../../../config/stakingConfig';
import smashSettingsIcon from '../../../assets/images/smash-settings-ic.inline.svg';
import tinySpinnerIcon from '../../../assets/images/spinner-tiny.inline.svg';

const messages = defineMessages({
  delegatingListTitle: {
    id: 'staking.stakePools.delegatingListTitle',
    defaultMessage: '!!!Stake pools to which you are delegating',
    description: '"delegatingListTitle" for the Stake Pools page.',
  },
  listTitle: {
    id: 'staking.stakePools.listTitle',
    defaultMessage: '!!!Stake pools',
    description: '"listTitle" for the Stake Pools page.',
  },
  listTitleLoading: {
    id: 'staking.stakePools.listTitleLoading',
    defaultMessage: '!!!Loading stake pools',
    description: '"listTitleLoading" for the Stake Pools page.',
  },
  listTitleSearch: {
    id: 'staking.stakePools.listTitleSearch',
    defaultMessage: '!!!Stake pools. Search results:',
    description: '"listTitleSearch" for the Stake Pools page.',
  },
  listTitleStakePools: {
    id: 'staking.stakePools.listTitleStakePools',
    defaultMessage: '!!!({pools})',
    description: '"listTitleStakePools" for the Stake Pools page.',
  },
  loadingStakePoolsMessage: {
    id: 'staking.stakePools.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
  moderatedBy: {
    id: 'staking.stakePools.moderatedBy',
    defaultMessage: '!!!Moderated by',
    description: 'moderatedBy message for the Delegation center body section.',
  },
  unmoderated: {
    id: 'staking.stakePools.unmoderated',
    defaultMessage: '!!!Unmoderated',
    description: 'unmoderated message for the Delegation center body section.',
  },
  emptyFilterWarningPrefix: {
    id: 'staking.stakePools.filter.emptyFilterWarningPrefix',
    defaultMessage: '!!!Please select at least one category in the filter or',
    description: 'Empty filter warning prefix',
  },
  emptyFilterWarningSuffix: {
    id: 'staking.stakePools.filter.emptyFilterWarningSuffix',
    defaultMessage: '!!!.',
    description: 'Empty filter warning suffix',
  },
  resetFilter: {
    id: 'staking.stakePools.filter.resetFilter',
    defaultMessage: '!!!Reset Filter',
    description: 'Reset Filter link label',
  },
});

const SELECTED_INDEX_TABLE = 'selectedIndexTable';

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
  isFetching: boolean,
  isRanking: boolean,
  stakePoolsDelegatingList: Array<StakePool>,
  getStakePoolById: Function,
  onSmashSettingsClick: Function,
  smashServerUrl: ?string,
  maxDelegationFunds: number,
  filterOptions: StakePoolFilterOptionsType,
  onFilter: Function,
  populatedFilterOptions: StakePoolFilterOptionsType,
};

type State = {
  search: string,
  selectedList?: ?string,
  isGridView: boolean,
  isGridRewardsView: boolean,
  isListView: boolean,
  isTableHeaderHovered: boolean,
};

const initialState = {
  search: '',
  selectedList: null,
  isGridView: true,
  isGridRewardsView: false,
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
      isGridRewardsView: false,
      isListView: false,
    });

  handleGridRewardsView = () => {
    this.setState({
      isGridView: false,
      isGridRewardsView: true,
      isListView: false,
    });
  };

  handleListView = () =>
    this.setState({
      isGridView: false,
      isGridRewardsView: false,
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

  resetFilter = () => {
    const { onFilter } = this.props;
    onFilter(defaultStakePoolFilterOptions);
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
      isFetching,
      isRanking,
      stakePoolsDelegatingList,
      getStakePoolById,
      smashServerUrl,
      onSmashSettingsClick,
      maxDelegationFunds,
      filterOptions,
      onFilter,
      populatedFilterOptions,
    } = this.props;
    const {
      search,
      selectedList,
      isListView,
      isGridView,
      isGridRewardsView,
      isTableHeaderHovered,
    } = this.state;

    const numberOfFilterDimensionsApplied = getNumberOfFilterDimensionsApplied(
      filterOptions
    );

    const filteredStakePoolsList: Array<StakePool> = getFilteredStakePoolsList(
      stakePoolsList,
      search
    );
    const numberOfRankedStakePools = filteredStakePoolsList.filter(
      (stakePool) =>
        IS_RANKING_DATA_AVAILABLE && stakePool.nonMyopicMemberRewards
    ).length;

    const isFilterDisabled =
      !filteredStakePoolsList.length &&
      numberOfFilterDimensionsApplied === Object.keys(filterOptions).length;

    const listTitleMessage = isFetching
      ? messages.listTitleLoading
      : messages.listTitle;

    const listTitleSearchMessage =
      !!search.trim().length && intl.formatMessage(messages.listTitleSearch);

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

    const smashServer = smashServerUrl
      ? getSmashServerNameFromUrl(smashServerUrl)
      : null;

    const tinyLoadingSpinner = isFetching && (
      <SVGInline svg={tinySpinnerIcon} className={styles.tinySpinner} />
    );

    const smashSettings = (
      <button onClick={onSmashSettingsClick} className={styles.smashSettings}>
        <span>
          {smashServer && smashServer !== SMASH_SERVER_TYPES.DIRECT
            ? intl.formatMessage(messages.moderatedBy, { smashServer })
            : intl.formatMessage(messages.unmoderated)}
        </span>
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
              onOpenExternalLink={onOpenExternalLink}
              updateDelegatingStake={updateDelegatingStake}
              rankStakePools={rankStakePools}
              selectedDelegationWalletId={selectedDelegationWalletId}
              stake={stake}
              isLoading={isLoading}
              isRanking={isRanking}
              numberOfStakePools={stakePoolsList.length}
              getStakePoolById={getStakePoolById}
              maxDelegationFunds={maxDelegationFunds}
              maxDelegationFundsLog={Math.log(maxDelegationFunds)}
            />
            <StakePoolsSearch
              search={search}
              onSearch={this.handleSearch}
              onClearSearch={this.handleClearSearch}
              onGridView={this.handleGridView}
              onGridRewardsView={this.handleGridRewardsView}
              onListView={this.handleListView}
              isListView={isListView}
              isGridView={isGridView}
              isGridRewardsView={isGridRewardsView}
              smashServer={smashServer}
              filterPopOverProps={{
                populatedFilterOptions,
                onFilter,
                isFilterDisabled,
                numberOfFilterDimensionsApplied,
              }}
            />
            {stakePoolsDelegatingList.length > 0 && (
              <Fragment>
                <h2 className={styles.listTitle}>
                  <span className={styles.leftContent}>
                    <span className={styles.blurText}>
                      {intl.formatMessage(messages.delegatingListTitle)}
                    </span>
                  </span>
                </h2>
                <StakingPageScrollContext.Consumer>
                  {(stakePoolsScrollContext) => (
                    <StakePoolsList
                      stakePoolsList={stakePoolsDelegatingList}
                      onOpenExternalLink={onOpenExternalLink}
                      currentTheme={currentTheme}
                      containerClassName="StakingWithNavigation_page"
                      onSelect={this.onDelegate}
                      numberOfRankedStakePools={numberOfRankedStakePools}
                      isGridRewardsView={isGridRewardsView}
                      showWithSelectButton
                      scrollElementRef={
                        stakePoolsScrollContext.scrollElementRef
                      }
                    />
                  )}
                </StakingPageScrollContext.Consumer>
              </Fragment>
            )}
            {isListView && (
              <Fragment>
                <h2>
                  <span className={styles.leftContent}>
                    {numberOfFilterDimensionsApplied === 0 ? (
                      <span className={styles.emptyFilterWarning}>
                        <span className={styles.blurText}>
                          {intl.formatMessage(
                            messages.emptyFilterWarningPrefix
                          )}
                        </span>
                        <Link
                          className={styles.resetFilter}
                          onClick={this.resetFilter}
                          label={intl.formatMessage(messages.resetFilter)}
                        />
                        <span className={styles.blurText}>
                          {intl.formatMessage(
                            messages.emptyFilterWarningSuffix
                          )}
                        </span>
                      </span>
                    ) : (
                      <span className={styles.blurText}>
                        {intl.formatMessage(listTitleMessage)}
                        {listTitleSearchMessage}
                        {intl.formatMessage(messages.listTitleStakePools, {
                          pools: formattedNumber(filteredStakePoolsList.length),
                        })}
                      </span>
                    )}
                    {tinyLoadingSpinner}
                  </span>
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
            {(isGridView || isGridRewardsView) && (
              <Fragment>
                <h2>
                  <span className={styles.leftContent}>
                    {numberOfFilterDimensionsApplied === 0 ? (
                      <span className={styles.emptyFilterWarning}>
                        <span className={styles.blurText}>
                          {intl.formatMessage(
                            messages.emptyFilterWarningPrefix
                          )}
                        </span>
                        <Link
                          className={styles.resetFilter}
                          onClick={this.resetFilter}
                          label={intl.formatMessage(messages.resetFilter)}
                        />
                        <span className={styles.blurText}>
                          {intl.formatMessage(
                            messages.emptyFilterWarningSuffix
                          )}
                        </span>
                      </span>
                    ) : (
                      <span className={styles.blurText}>
                        {intl.formatMessage(listTitleMessage)}
                        {listTitleSearchMessage}
                        {intl.formatMessage(messages.listTitleStakePools, {
                          pools: formattedNumber(filteredStakePoolsList.length),
                        })}
                      </span>
                    )}
                    {tinyLoadingSpinner}
                  </span>
                  {smashSettings}
                </h2>
                <StakingPageScrollContext.Consumer>
                  {(stakePoolsScrollContext) => (
                    <StakePoolsList
                      showWithSelectButton
                      stakePoolsList={filteredStakePoolsList}
                      onOpenExternalLink={onOpenExternalLink}
                      currentTheme={currentTheme}
                      containerClassName="StakingWithNavigation_page"
                      onSelect={this.onDelegate}
                      numberOfRankedStakePools={numberOfRankedStakePools}
                      isGridRewardsView={isGridRewardsView}
                      scrollElementRef={
                        stakePoolsScrollContext.scrollElementRef
                      }
                    />
                  )}
                </StakingPageScrollContext.Consumer>
              </Fragment>
            )}
          </Fragment>
        )}
      </div>
    );
  }
}
