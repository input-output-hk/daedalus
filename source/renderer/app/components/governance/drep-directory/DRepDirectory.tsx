import React, { useEffect, useMemo, useRef, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepDirectoryBanner from './DRepDirectoryBanner';
import DRepDirectorySearch from './DRepDirectorySearch';
import DRepDirectoryFilters from './DRepDirectoryFilters';
import DRepEmptyState from '../_shared/DRepEmptyState';
import DRepErrorBanner from '../_shared/DRepErrorBanner';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
  DRepCohortContext,
} from '../../../stores/GovernanceStore';
import { GovernanceQueryErrorType } from '../../../../../common/types/governance.types';
import {
  DEFAULT_DREP_FILTER_STATE,
  EMPTY_DREP_ID_SET,
  buildDRepSearchIndex,
  filterDReps,
  getDRepQueryKind,
  isDefaultFilterState,
  resolveExactDRepMatch,
  searchDRepsByIdPrefix,
  sortDReps,
} from './helpers';
import type { DRepFilterState, DRepSortOption } from './helpers';
import styles from './DRepDirectory.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title of the DRep directory page',
  },
  loading: {
    id: 'governance.drepDirectory.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state message',
  },
  empty: {
    id: 'governance.drepDirectory.empty',
    defaultMessage: '!!!No DReps found on-chain.',
    description: 'Empty directory state',
  },
  error: {
    id: 'governance.drepDirectory.error',
    defaultMessage: '!!!Could not load DRep data.',
    description: 'Error state message',
  },
  retry: {
    id: 'governance.drepDirectory.retry',
    defaultMessage: '!!!Retry',
    description: 'Retry button label',
  },
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Refreshing state badge label',
  },
  syncing: {
    id: 'governance.drepDirectory.syncing',
    defaultMessage:
      '!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.',
    description: 'Persistent soft-warning banner while the node is syncing',
  },
  sortBiasWarning: {
    id: 'governance.drepDirectory.showAll.sortBiasWarning',
    defaultMessage:
      '!!!Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing.',
    description:
      'Disclosure shown while voting-power-descending sort is active',
  },
});

interface Props {
  drepList: AppDRepDirectoryEntry[];
  drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>;
  showAllList: AppDRepDirectoryEntry[];
  top35DRepIds: ReadonlySet<string>;
  cohort: DRepCohortContext;
  favoriteDRepIds?: ReadonlySet<string>;
  view?: 'directory' | 'favorites';
  onToggleFavorite: (drepId: string) => void;
  onBackToDirectory?: () => void;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  isNodeInSync: boolean;
  syncProgress: number | null;
  votingPowerState: VotingPowerEnrichState;
  isCohortActive: boolean;
  onRefresh: () => void;
  onReshuffle: () => void;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  drepList,
  drepIndex,
  showAllList,
  top35DRepIds,
  cohort,
  favoriteDRepIds = EMPTY_DREP_ID_SET,
  view = 'directory',
  onToggleFavorite,
  onBackToDirectory,
  isStaleFavoriteEntry,
  refreshState,
  error,
  lastFetchedAt,
  isNodeInSync,
  syncProgress,
  votingPowerState,
  isCohortActive,
  onRefresh,
  onReshuffle,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const [searchQuery, setSearchQuery] = useState('');
  const [isShowAll, setIsShowAll] = useState(false);
  const [filters, setFilters] = useState<DRepFilterState>(
    DEFAULT_DREP_FILTER_STATE
  );
  const [sort, setSort] = useState<DRepSortOption>('randomized');

  const queryKind = getDRepQueryKind(searchQuery);
  const isSearchActive =
    queryKind === 'prefix' ||
    queryKind === 'exactValid' ||
    queryKind === 'invalidFullForm';
  const isRankingAvailable = votingPowerState === VotingPowerEnrichState.Loaded;
  const isFavoritesView = view === 'favorites';

  // Search always covers the full membership so excluded and non-cohort
  // DReps stay reachable regardless of the current view.
  const searchIndex = useMemo(
    () => buildDRepSearchIndex(showAllList),
    [showAllList]
  );

  const visibleEntries = useMemo(() => {
    let base: AppDRepDirectoryEntry[];
    if (isSearchActive) {
      base = searchDRepsByIdPrefix(searchIndex, searchQuery);
    } else if (isShowAll) {
      base = showAllList;
    } else {
      base = drepList;
    }
    const filtered = filterDReps(base, filters, {
      favoriteDRepIds,
      top35DRepIds,
    });
    // Search results keep relevance order; the cohort keeps its seeded order.
    if (isSearchActive || !isShowAll) return filtered;
    return sortDReps(filtered, sort);
  }, [
    drepList,
    favoriteDRepIds,
    filters,
    isSearchActive,
    isShowAll,
    searchIndex,
    searchQuery,
    showAllList,
    sort,
    top35DRepIds,
  ]);

  // The favorites view reuses the slice-6 favorited predicate over the full
  // membership, so favorited entries outside the default cohort stay visible.
  const favoritesEntries = useMemo(
    () =>
      filterDReps(
        showAllList,
        { ...DEFAULT_DREP_FILTER_STATE, favoritedOnly: true },
        { favoriteDRepIds, top35DRepIds }
      ),
    [favoriteDRepIds, showAllList, top35DRepIds]
  );

  // A checksum-valid full ID that resolves in the index bypasses the result
  // list and opens the detail view directly, once per distinct query.
  const lastOpenedQueryRef = useRef<string | null>(null);
  useEffect(() => {
    if (isFavoritesView || queryKind !== 'exactValid') return;
    if (lastOpenedQueryRef.current === searchQuery) return;
    const match = resolveExactDRepMatch(searchQuery, drepIndex);
    if (match) {
      lastOpenedQueryRef.current = searchQuery;
      onViewDetails(match.drepId);
    }
  }, [isFavoritesView, queryKind, searchQuery, drepIndex, onViewDetails]);

  const isFilteredView =
    isSearchActive ||
    isShowAll ||
    sort !== 'randomized' ||
    !isDefaultFilterState(filters);

  const handleShowAllChange = (nextShowAll: boolean) => {
    setIsShowAll(nextShowAll);
    // Sorts exist only under show-all; leaving it restores the default order.
    if (!nextShowAll) setSort('randomized');
  };

  const handleClearFilters = () => {
    setSearchQuery('');
    setFilters(DEFAULT_DREP_FILTER_STATE);
    setSort('randomized');
  };

  const handleShowAllFromEmptyState = () => {
    handleClearFilters();
    setIsShowAll(true);
  };

  const hasRetainedData = showAllList.length > 0;
  const showErrorBanner = error && hasRetainedData;

  // While syncing, an empty or unavailable directory is expected — fall back
  // to the noSync empty state instead of a bare error or "No DReps found".
  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    (refreshState === GovernanceRefreshState.Loaded ||
      (refreshState === GovernanceRefreshState.Failed &&
        error?.type !== GovernanceQueryErrorType.SelfnodeCliUnsupported));

  const renderContent = () => {
    switch (true) {
      case refreshState === GovernanceRefreshState.Loading:
        return (
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        );

      case showNoSyncFallback:
        return <DRepEmptyState variant="noSync" />;

      case refreshState === GovernanceRefreshState.Failed:
        return (
          <div className={styles.stateContainer}>
            <p className={styles.errorMessage}>
              {intl.formatMessage(messages.error)}
            </p>
            {error && <p className={styles.errorDetails}>{error.message}</p>}
            {error?.details && (
              <p className={styles.errorDetails}>{error.details}</p>
            )}
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      case showAllList.length === 0 &&
        refreshState === GovernanceRefreshState.Loaded:
        return (
          <div className={styles.stateContainer}>
            <p>{intl.formatMessage(messages.empty)}</p>
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      default:
        if (isFavoritesView) {
          return favoritesEntries.length === 0 ? (
            <DRepEmptyState
              variant="noFavorites"
              onBackToDirectory={onBackToDirectory}
            />
          ) : (
            <DRepDirectoryList
              entries={favoritesEntries}
              cohort={cohort}
              favoriteDRepIds={favoriteDRepIds}
              onToggleFavorite={onToggleFavorite}
              isFavoritesView
              isStaleFavoriteEntry={isStaleFavoriteEntry}
              onSelectForDelegation={onSelectForDelegation}
              onViewDetails={onViewDetails}
              votingPowerState={votingPowerState}
            />
          );
        }
        return (
          <>
            <div className={styles.controlsRow}>
              <DRepDirectorySearch
                value={searchQuery}
                queryKind={queryKind}
                onChange={setSearchQuery}
              />
              <DRepDirectoryFilters
                filters={filters}
                onFiltersChange={setFilters}
                isShowAll={isShowAll}
                onShowAllChange={handleShowAllChange}
                sort={sort}
                onSortChange={setSort}
                isRankingAvailable={isRankingAvailable}
                isSearchActive={isSearchActive}
              />
            </div>
            {showErrorBanner && error && (
              <div className={styles.errorBanner}>
                <div>
                  <p className={styles.errorMessage}>
                    {intl.formatMessage(messages.error)}
                  </p>
                  <p className={styles.errorDetails}>{error.message}</p>
                  {error.details && (
                    <p className={styles.errorDetails}>{error.details}</p>
                  )}
                </div>
                <Button
                  label={intl.formatMessage(messages.retry)}
                  onClick={onRefresh}
                  skin={ButtonSkin}
                />
              </div>
            )}
            {refreshState === GovernanceRefreshState.Refreshing && (
              <div className={styles.refreshingBadge}>
                <LoadingSpinner />
                {intl.formatMessage(messages.refreshing)}
              </div>
            )}
            {votingPowerState === VotingPowerEnrichState.Failed && (
              <DRepErrorBanner variant="rankingUnavailable" />
            )}
            {isShowAll && !isSearchActive && sort === 'votingPowerDesc' && (
              <div className={styles.sortBiasWarning} role="status">
                {intl.formatMessage(messages.sortBiasWarning)}
              </div>
            )}
            {visibleEntries.length === 0 ? (
              <DRepEmptyState
                variant="noResults"
                onClearFilters={handleClearFilters}
                onShowAll={handleShowAllFromEmptyState}
              />
            ) : (
              <DRepDirectoryList
                entries={visibleEntries}
                isSearchResult={isSearchActive}
                cohort={cohort}
                favoriteDRepIds={favoriteDRepIds}
                onToggleFavorite={onToggleFavorite}
                onSelectForDelegation={onSelectForDelegation}
                onViewDetails={onViewDetails}
                votingPowerState={votingPowerState}
              />
            )}
          </>
        );
    }
  };

  return (
    <div className={styles.container}>
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
        isCohortActive={isCohortActive}
        onReshuffle={onReshuffle}
        isFilteredView={isFilteredView}
        displayedCount={visibleEntries.length}
        isFavoritesView={isFavoritesView}
        favoritesCount={favoritesEntries.length}
      />
      {!isNodeInSync && (
        <div className={styles.syncingBanner} role="status">
          <svg
            className={styles.syncingIcon}
            aria-hidden="true"
            width="16"
            height="16"
            viewBox="0 0 16 16"
          >
            <path
              d="M8 1.5 15 14H1L8 1.5z"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.5"
              strokeLinejoin="round"
            />
            <path d="M8 6v4" stroke="currentColor" strokeWidth="1.5" />
            <circle cx="8" cy="12" r="0.9" fill="currentColor" />
          </svg>
          <span>
            {intl.formatMessage(messages.syncing, {
              progress: Math.floor(syncProgress ?? 0),
            })}
          </span>
        </div>
      )}
      {renderContent()}
    </div>
  );
}

export default injectIntl(DRepDirectory);
