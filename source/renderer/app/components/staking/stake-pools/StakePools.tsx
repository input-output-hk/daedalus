import React, { Component, Fragment } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import { debounce } from 'lodash';
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
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  SMASH_SERVER_TYPES,
} from '../../../config/stakingConfig';
import smashSettingsIcon from '../../../assets/images/smash-settings-ic.inline.svg';
import tinySpinnerIcon from '../../../assets/images/spinner-tiny.inline.svg';
import { getSmashServerNameFromUrl } from '../../../utils/staking';
import { AnalyticsTracker, EventCategories } from '../../../analytics';

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
});
const SELECTED_INDEX_TABLE = 'selectedIndexTable';
type Props = {
  analyticsTracker: AnalyticsTracker;
  currentLocale: string;
  currentTheme: string;
  getStakePoolById: (...args: Array<any>) => any;
  isFetching: boolean;
  isListViewTooltipVisible?: boolean;
  isLoading: boolean;
  isRanking: boolean;
  maxDelegationFunds: number;
  onDelegate: (...args: Array<any>) => any;
  onListViewVisited: () => void;
  onOpenExternalLink: (...args: Array<any>) => any;
  onSmashSettingsClick: (...args: Array<any>) => any;
  rankStakePools: (...args: Array<any>) => any;
  selectedDelegationWalletId?: string | null | undefined;
  smashServerUrl: string | null | undefined;
  stake?: number | null | undefined;
  stakePoolsDelegatingList: Array<StakePool>;
  stakePoolsList: Array<StakePool>;
  updateDelegatingStake: (...args: Array<any>) => any;
  wallets: Array<Wallet>;
};
type State = {
  search: string;
  selectedList?: string | null | undefined;
  isGridView: boolean;
  isGridRewardsView: boolean;
  isListView: boolean;
  isTableHeaderHovered: boolean;
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
class StakePools extends Component<Props, State> {
  loadingSpinner: LoadingSpinner | null | undefined;
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = { ...initialState };

  sendSearchAnalyticsEvent = debounce(
    () =>
      this.props.analyticsTracker.sendEvent(
        EventCategories.STAKE_POOLS,
        'Used stake pools search'
      ),
    5000
  );

  handleSearch = (search: string) => {
    this.setState({
      search,
    });

    this.sendSearchAnalyticsEvent();
  };

  handleClearSearch = () =>
    this.setState({
      search: '',
    });

  handleGridView = () => {
    this.setState({
      isGridView: true,
      isGridRewardsView: false,
      isListView: false,
    });

    this.props.analyticsTracker.sendEvent(
      EventCategories.STAKE_POOLS,
      'Changed view to grid view'
    );
  };
  handleGridRewardsView = () => {
    this.setState({
      isGridView: false,
      isGridRewardsView: true,
      isListView: false,
    });

    this.props.analyticsTracker.sendEvent(
      EventCategories.STAKE_POOLS,
      'Changed view to grid rewards view'
    );
  };
  handleListView = () => {
    this.setState({
      isGridView: false,
      isGridRewardsView: false,
      isListView: true,
    });

    this.props.analyticsTracker.sendEvent(
      EventCategories.STAKE_POOLS,
      'Changed view to list view'
    );
  };
  handleSetListActive = (selectedList: string) =>
    this.setState({
      selectedList,
    });
  handleTableHeaderMouseEnter = () =>
    this.setState({
      isTableHeaderHovered: true,
    });
  handleTableHeaderMouseLeave = () =>
    this.setState({
      isTableHeaderHovered: false,
    });
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
      onListViewVisited,
      currentTheme,
      isFetching,
      isListViewTooltipVisible,
      isLoading,
      isRanking,
      stakePoolsDelegatingList,
      getStakePoolById,
      smashServerUrl,
      onSmashSettingsClick,
      maxDelegationFunds,
    } = this.props;
    const {
      search,
      selectedList,
      isListView,
      isGridView,
      isGridRewardsView,
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
            ? intl.formatMessage(messages.moderatedBy, {
                smashServer,
              })
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
              onListViewVisited={onListViewVisited}
              isListView={isListView}
              isListViewTooltipVisible={isListViewTooltipVisible}
              isGridView={isGridView}
              isGridRewardsView={isGridRewardsView}
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              smashServer={smashServer}
            />
            {stakePoolsDelegatingList.length > 0 && (
              <Fragment>
                <h2 className={styles.listTitle}>
                  <span className={styles.leftContent}>
                    <span>
                      {intl.formatMessage(messages.delegatingListTitle)}
                    </span>
                  </span>
                </h2>
                {isListView ? (
                  <StakePoolsTable
                    listName={SELECTED_INDEX_TABLE}
                    currentLocale={currentLocale}
                    stakePoolsList={stakePoolsDelegatingList}
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
                ) : (
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
                )}
              </Fragment>
            )}
            {isListView && (
              <Fragment>
                <h2>
                  <span className={styles.leftContent}>
                    <span>
                      {intl.formatMessage(listTitleMessage)}
                      {listTitleSearchMessage}
                      {intl.formatMessage(messages.listTitleStakePools, {
                        pools: formattedNumber(filteredStakePoolsList.length),
                      })}
                    </span>
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
                    <span>
                      {intl.formatMessage(listTitleMessage)}
                      {listTitleSearchMessage}
                      {intl.formatMessage(messages.listTitleStakePools, {
                        pools: formattedNumber(filteredStakePoolsList.length),
                      })}
                    </span>
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

export default StakePools;
