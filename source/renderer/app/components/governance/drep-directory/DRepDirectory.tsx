import React, { useEffect, useMemo, useRef, useState } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepDirectoryBanner from './DRepDirectoryBanner';
import DRepDirectorySearch from './DRepDirectorySearch';
import DRepDirectoryFilters from './DRepDirectoryFilters';
import DRepDirectorySkeleton from './DRepDirectorySkeleton';
import DRepEmptyState from '../_shared/DRepEmptyState';
import DRepErrorBanner from '../_shared/DRepErrorBanner';
import {
  GovernanceRefreshState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
} from '../../../stores/GovernanceStore';
import {
  DEFAULT_DREP_FILTER_STATE,
  EMPTY_DREP_ID_SET,
  buildDRepSearchIndex,
  filterDReps,
  getDRepQueryKind,
  isDefaultFilterState,
  resolveExactDRepMatch,
  searchDRepsByIdPrefix,
  searchDRepsByName,
  sortDReps,
} from './helpers';
import type { DRepFilterState, DRepSortOption } from './helpers';
import { sharedGovernanceMessages } from '../../voting/voting-governance/shared-messages';
import styles from './DRepDirectory.scss';

const messages = defineMessages({
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
  reroll: {
    id: 'governance.drepDirectory.reroll',
    defaultMessage: '!!!Show different suggestions',
    description: 'Button to fetch a fresh set of randomly suggested DReps',
  },
  orChoosePredefined: {
    id: 'governance.drepDirectory.orChoosePredefined',
    defaultMessage: '!!!Or choose a predefined option:',
    description: 'Label above the Abstain / No Confidence sentinel cards',
  },
  abstainDescription: {
    id: 'governance.drepDirectory.abstain.description',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.',
    description: 'Description shown on the Abstain sentinel card',
  },
  noConfidenceDescription: {
    id: 'governance.drepDirectory.noConfidence.description',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.',
    description: 'Description shown on the No Confidence sentinel card',
  },
  selectOption: {
    id: 'governance.drepDirectory.sentinelCard.select',
    defaultMessage: '!!!Select',
    description: 'CTA on the Abstain / No Confidence sentinel cards',
  },
});

interface Props {
  suggestedDReps: AppDRepDirectoryEntry[];
  allDReps: AppDRepDirectoryEntry[];
  allDRepsRefreshState: GovernanceRefreshState;
  favoriteDRepIds?: ReadonlySet<string>;
  view?: 'directory' | 'favorites';
  onToggleFavorite: (drepId: string) => void;
  onBackToDirectory?: () => void;
  isStaleFavoriteEntry?: (entry: AppDRepDirectoryEntry) => boolean;
  isCurrentDRep?: (entry: AppDRepDirectoryEntry) => boolean;
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  isNodeInSync: boolean;
  syncProgress: number | null;
  onRefresh: () => void;
  onReroll: () => void;
  onLoadAllDReps: () => void;
  canDelegate?: boolean;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  suggestedDReps,
  allDReps,
  allDRepsRefreshState,
  favoriteDRepIds = EMPTY_DREP_ID_SET,
  view = 'directory',
  onToggleFavorite,
  onBackToDirectory,
  isStaleFavoriteEntry,
  isCurrentDRep,
  refreshState,
  error,
  lastFetchedAt,
  isNodeInSync,
  syncProgress,
  onRefresh,
  onReroll,
  onLoadAllDReps,
  canDelegate = true,
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
    queryKind === 'invalidFullForm' ||
    queryKind === 'name';
  const isRankingAvailable = true;
  const isFavoritesView = view === 'favorites';

  // Trigger lazy load of the full list for favorites view, show-all, or search.
  useEffect(() => {
    if (isFavoritesView || isShowAll || isSearchActive) {
      onLoadAllDReps();
    }
  }, [isFavoritesView, isShowAll, isSearchActive, onLoadAllDReps]);

  const searchIndex = useMemo(() => buildDRepSearchIndex(allDReps), [allDReps]);

  const allDRepsIndex = useMemo(
    () => new Map(allDReps.map((e) => [e.drepId, e])),
    [allDReps]
  );

  const visibleEntries = useMemo(() => {
    let base: AppDRepDirectoryEntry[];
    if (isSearchActive) {
      base =
        queryKind === 'name'
          ? searchDRepsByName(allDReps, searchQuery)
          : searchDRepsByIdPrefix(searchIndex, searchQuery);
    } else if (isShowAll) {
      base = allDReps;
    } else {
      base = suggestedDReps;
    }
    const filtered = filterDReps(base, filters, { favoriteDRepIds });
    if (isSearchActive || !isShowAll) return filtered;
    return sortDReps(filtered, sort);
  }, [
    suggestedDReps,
    favoriteDRepIds,
    filters,
    isSearchActive,
    isShowAll,
    queryKind,
    searchIndex,
    searchQuery,
    allDReps,
    sort,
  ]);

  const favoritesEntries = useMemo(
    () =>
      filterDReps(
        allDReps,
        { ...DEFAULT_DREP_FILTER_STATE, favoritedOnly: true },
        { favoriteDRepIds }
      ),
    [favoriteDRepIds, allDReps]
  );

  const lastOpenedQueryRef = useRef<string | null>(null);
  useEffect(() => {
    if (isFavoritesView || queryKind !== 'exactValid') return;
    if (lastOpenedQueryRef.current === searchQuery) return;
    const match = resolveExactDRepMatch(searchQuery, allDRepsIndex);
    if (match) {
      lastOpenedQueryRef.current = searchQuery;
      onViewDetails(match.drepId);
    }
  }, [isFavoritesView, queryKind, searchQuery, allDRepsIndex, onViewDetails]);

  const isFilteredView =
    isSearchActive ||
    isShowAll ||
    sort !== 'randomized' ||
    !isDefaultFilterState(filters);

  const handleShowAllChange = (nextShowAll: boolean) => {
    setIsShowAll(nextShowAll);
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

  const hasRetainedData = suggestedDReps.length > 0;
  const showErrorBanner = error && hasRetainedData;

  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    (refreshState === GovernanceRefreshState.Loaded ||
      refreshState === GovernanceRefreshState.Failed);

  const renderContent = () => {
    switch (true) {
      case refreshState === GovernanceRefreshState.Loading:
        return <DRepDirectorySkeleton />;

      case error?.type === 'SELFNODE_CLI_UNSUPPORTED':
        return <DRepEmptyState variant="selfnode" />;

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

      case !isFavoritesView &&
        suggestedDReps.length === 0 &&
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
              favoriteDRepIds={favoriteDRepIds}
              onToggleFavorite={onToggleFavorite}
              isFavoritesView
              isStaleFavoriteEntry={isStaleFavoriteEntry}
              isCurrentDRep={isCurrentDRep}
              canDelegate={canDelegate}
              onSelectForDelegation={onSelectForDelegation}
              onViewDetails={onViewDetails}
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
            {showErrorBanner && (
              <DRepErrorBanner
                variant="refreshFailed"
                retryLabel={intl.formatMessage(messages.retry)}
                onRetry={onRefresh}
                lastFetchedAt={lastFetchedAt}
              />
            )}
            {isShowAll && !isSearchActive && sort === 'votingPowerDesc' && (
              <div className={styles.sortBiasWarning} role="status">
                {intl.formatMessage(messages.sortBiasWarning)}
              </div>
            )}
            {isShowAll &&
              allDRepsRefreshState === GovernanceRefreshState.Loading && (
                <DRepDirectorySkeleton />
              )}
            {visibleEntries.length === 0 &&
            allDRepsRefreshState !== GovernanceRefreshState.Loading ? (
              <DRepEmptyState
                variant="noResults"
                onClearFilters={handleClearFilters}
                onShowAll={handleShowAllFromEmptyState}
              />
            ) : (
              <DRepDirectoryList
                entries={visibleEntries}
                isSearchResult={isSearchActive}
                favoriteDRepIds={favoriteDRepIds}
                onToggleFavorite={onToggleFavorite}
                isCurrentDRep={isCurrentDRep}
                canDelegate={canDelegate}
                onSelectForDelegation={onSelectForDelegation}
                onViewDetails={onViewDetails}
              />
            )}
            {!isSearchActive && !isShowAll && canDelegate && (
              <div className={styles.suggestedFooter}>
                <Button
                  label={intl.formatMessage(messages.reroll)}
                  onClick={onReroll}
                  disabled={
                    refreshState === GovernanceRefreshState.Loading ||
                    refreshState === GovernanceRefreshState.Refreshing
                  }
                  skin={ButtonSkin}
                />
                <p className={styles.orChoosePredefined}>
                  {intl.formatMessage(messages.orChoosePredefined)}
                </p>
                <div className={styles.sentinelCards}>
                  <div className={styles.sentinelCard}>
                    <p className={styles.sentinelCardTitle}>
                      {intl.formatMessage(sharedGovernanceMessages.abstain)}
                    </p>
                    <p className={styles.sentinelCardDescription}>
                      {intl.formatMessage(messages.abstainDescription)}
                    </p>
                    <Button
                      label={intl.formatMessage(messages.selectOption)}
                      onClick={() => onSelectForDelegation('abstain')}
                      skin={ButtonSkin}
                    />
                  </div>
                  <div className={styles.sentinelCard}>
                    <p className={styles.sentinelCardTitle}>
                      {intl.formatMessage(
                        sharedGovernanceMessages.noConfidence
                      )}
                    </p>
                    <p className={styles.sentinelCardDescription}>
                      {intl.formatMessage(messages.noConfidenceDescription)}
                    </p>
                    <Button
                      label={intl.formatMessage(messages.selectOption)}
                      onClick={() => onSelectForDelegation('no_confidence')}
                      skin={ButtonSkin}
                    />
                  </div>
                </div>
              </div>
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
