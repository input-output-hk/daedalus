import React, { useEffect, useMemo, useRef, useState } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepCohortCriteriaPanel from './DRepCohortCriteriaPanel';
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
import {
  DEFAULT_DREP_COHORT_CRITERIA,
  orderDRepsByStanding,
} from '../_shared/drepCohort';
import type {
  DRepCohortCriteria,
  DRepCohortCriterion,
} from '../_shared/drepCohort';
import { sharedGovernanceMessages } from '../../voting/voting-governance/shared-messages';
// The same icons the stake pools view toggle uses.
// @ts-ignore inline svg module
import gridIcon from '../../../assets/images/grid-ic.inline.svg';
// @ts-ignore inline svg module
import listIcon from '../../../assets/images/list-ic.inline.svg';
import styles from './DRepDirectory.scss';
import type { ListViewMode } from '../../../types/listViewTypes';
import { DEFAULT_LIST_VIEW_MODE } from '../../../types/listViewTypes';

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
  viewModeCards: {
    id: 'governance.drepDirectory.viewMode.cards',
    defaultMessage: '!!!Card view',
    description: 'Accessible label of the card view toggle',
  },
  viewModeTable: {
    id: 'governance.drepDirectory.viewMode.table',
    defaultMessage: '!!!Table view',
    description: 'Accessible label of the table view toggle',
  },
  pinnedFavoritesTitle: {
    id: 'governance.drepDirectory.pinnedFavorites.title',
    defaultMessage: '!!!Your favorites ({count})',
    description: 'Heading of the favorites group pinned above the cohort',
  },
  sortBiasWarning: {
    id: 'governance.drepDirectory.showAll.sortBiasWarning',
    defaultMessage:
      '!!!Sorted by voting power, so the largest DReps come first. The default order randomises within each group instead, which is what stops a position near the top being worth holding.',
    description:
      'Disclosure shown while voting-power-descending sort is active',
  },
  relaxedNotice: {
    id: 'governance.drepDirectory.cohort.relaxedNotice',
    defaultMessage:
      '!!!Too few DReps met every suggestion criterion, so these were relaxed to fill the list: {criteria}.',
    description:
      'Disclosure shown when the eligible pool was too small for the cohort',
  },
  showAllSummary: {
    id: 'governance.drepDirectory.showAll.summary',
    defaultMessage:
      '!!!Showing {count, plural, one {# DRep} other {# DReps}} of {total}.',
    description: 'States how much of the full directory is on screen',
  },
  criteriaSummary: {
    id: 'governance.drepDirectory.cohort.summary',
    defaultMessage:
      '!!!Showing {count, plural, one {# DRep} other {# DReps}} drawn at random from the active DReps meeting these criteria.',
    description:
      'Lead line above the list of criteria the suggested cohort was drawn under',
  },
  criterionNotInactiveSoon: {
    id: 'governance.drepDirectory.cohort.criterion.notInactiveSoon',
    defaultMessage: '!!!not inactive soon',
    description: 'Name of the inactive-soon suggestion criterion',
  },
  criterionVotingPowerShare: {
    id: 'governance.drepDirectory.cohort.criterion.votingPowerShare',
    defaultMessage: '!!!voting power limit',
    description: 'Name of the voting-power ceiling suggestion criterion',
  },
  criterionVerifiedMetadata: {
    id: 'governance.drepDirectory.cohort.criterion.verifiedMetadata',
    defaultMessage: '!!!verified metadata',
    description: 'Name of the verified-metadata suggestion criterion',
  },
  reroll: {
    id: 'governance.drepDirectory.reroll',
    defaultMessage: '!!!Show different suggestions',
    description: 'Button to fetch a fresh set of randomly suggested DReps',
  },
  listTitleSuggested: {
    id: 'governance.drepDirectory.listTitle.suggested',
    defaultMessage: '!!!Suggested DReps',
    description: 'Label above the randomized cohort of DReps',
  },
  listTitleAll: {
    id: 'governance.drepDirectory.listTitle.all',
    defaultMessage: '!!!All DReps',
    description: 'Label above the full DRep list in show-all mode',
  },
  listTitleSearch: {
    id: 'governance.drepDirectory.listTitle.search',
    defaultMessage: '!!!DReps. Search results:',
    description: 'Label above the DRep list while a search is active',
  },
  listTitleCount: {
    id: 'governance.drepDirectory.listTitle.count',
    defaultMessage: '!!!({count})',
    description: 'Count appended to the DRep list label',
  },
  backToSuggestions: {
    id: 'governance.drepDirectory.showAll.back',
    defaultMessage: '!!!Back to suggestions',
    description: 'Button that narrows the full DRep list back to the cohort',
  },
  showAllCta: {
    id: 'governance.drepDirectory.showAll.cta',
    defaultMessage: '!!!Show all DReps',
    description: 'Button beside the reroll control that widens to every DRep',
  },
  orChoosePredefined: {
    id: 'governance.drepDirectory.orChoosePredefined',
    defaultMessage: '!!!Or choose a predefined option:',
    description: 'Label above the Abstain / No Confidence sentinel cards',
  },
  abstainDescription: {
    id: 'governance.drepDirectory.abstain.description',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance.',
    description: 'Description shown on the Abstain sentinel card',
  },
  noConfidenceDescription: {
    id: 'governance.drepDirectory.noConfidence.description',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no confidence, and as No on every other governance action.',
    description: 'Description shown on the No Confidence sentinel card',
  },
  selectOption: {
    id: 'governance.drepDirectory.sentinelCard.select',
    defaultMessage: '!!!Select',
    description: 'CTA on the Abstain / No Confidence sentinel cards',
  },
});

const criterionMessages: Record<
  DRepCohortCriterion,
  typeof messages.criterionNotInactiveSoon
> = {
  notInactiveSoon: messages.criterionNotInactiveSoon,
  votingPowerShare: messages.criterionVotingPowerShare,
  verifiedMetadata: messages.criterionVerifiedMetadata,
};

interface Props {
  suggestedDReps: AppDRepDirectoryEntry[];
  allDReps: AppDRepDirectoryEntry[];
  // The criteria the cohort was drawn under, and the means to change them.
  // Without a handler the panel is not offered: controls that cannot alter
  // what is on screen are worse than no controls.
  cohortCriteria?: DRepCohortCriteria;
  onCohortCriteriaChange?: (criteria: DRepCohortCriteria) => void;
  // Criteria that had to be given up to fill the cohort, in the order given
  // up. Empty whenever the pool satisfied every one of them.
  relaxedCohortCriteria?: DRepCohortCriterion[];
  favoriteDRepIds?: ReadonlySet<string>;
  favoriteEntries?: AppDRepDirectoryEntry[];
  // Seeds the search box. Lets a caller arrive at the directory already
  // looking for something, which is also the only way a story can show the
  // search behaving inside the directory rather than as a detached list.
  initialSearchQuery?: string;
  listViewMode?: ListViewMode;
  onListViewModeChange?: (mode: ListViewMode) => void;
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
  totalDRepStake?: BigNumber | null;
  epochLength?: number | null;
  slotLength?: number | null;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  suggestedDReps,
  allDReps,
  cohortCriteria = DEFAULT_DREP_COHORT_CRITERIA,
  onCohortCriteriaChange,
  relaxedCohortCriteria = [],
  favoriteDRepIds = new Set<string>() as ReadonlySet<string>,
  favoriteEntries = [],
  initialSearchQuery = '',
  listViewMode,
  onListViewModeChange,
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
  totalDRepStake = null,
  epochLength = null,
  slotLength = null,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const [searchQuery, setSearchQuery] = useState(initialSearchQuery);
  // Fixed for the life of the view: the bands must not re-shuffle under a
  // reader every time an unrelated piece of state changes.
  const [orderSeed] = useState(1);
  const [isShowAll, setIsShowAll] = useState(false);
  // Seeded from the stored preference, which is shared with the stake pools
  // screen so the two cannot disagree, but held locally as well so the toggle
  // responds immediately and the component still works without a store behind
  // it.
  const [viewMode, setLocalViewMode] = useState<ListViewMode>(
    listViewMode ?? DEFAULT_LIST_VIEW_MODE
  );

  useEffect(() => {
    if (listViewMode != null && listViewMode !== viewMode) {
      setLocalViewMode(listViewMode);
    }
    // Deliberately keyed on the stored preference alone: this re-seeds when
    // the stored value changes, and must not fire when the local view does.
  }, [listViewMode]);

  const setViewMode = (mode: ListViewMode) => {
    setLocalViewMode(mode);
    onListViewModeChange?.(mode);
  };
  const [filters, setFilters] = useState<DRepFilterState>(
    DEFAULT_DREP_FILTER_STATE
  );
  const [sort, setSort] = useState<DRepSortOption>('default');

  const queryKind = getDRepQueryKind(searchQuery);
  const isSearchActive =
    queryKind === 'prefix' ||
    queryKind === 'exactValid' ||
    queryKind === 'invalidFullForm' ||
    queryKind === 'name';
  // Both voting-power sorts collapse to the same identifier-ordered list when
  // no entry carries a figure, so offering them would be two controls that do
  // nothing and disagree with their own labels about it. Read from the whole
  // population rather than the visible list: a filter that happens to exclude
  // every entry holding a figure is not the same as the network not reporting
  // one.
  const isRankingAvailable = useMemo(
    () => allDReps.some((entry) => entry.votingPower != null),
    [allDReps]
  );
  const isFavoritesView = view === 'favorites';

  // Every view of the directory is drawn from the full list, the suggested
  // cohort included, so it is loaded on open rather than when a control
  // widens the view. Selecting the cohort here is what makes the criteria
  // ours to state and to change; it cannot be done from a list we do not
  // have.
  //
  // Asked for once per mount rather than once per render: the callback is
  // rebuilt on every render of the page above, and a network that legitimately
  // answers with no DReps would otherwise be asked again on each one.
  const hasRequestedAllDReps = useRef(false);
  useEffect(() => {
    if (hasRequestedAllDReps.current) return;
    hasRequestedAllDReps.current = true;
    onLoadAllDReps();
  }, [onLoadAllDReps]);

  const searchIndex = useMemo(() => buildDRepSearchIndex(allDReps), [allDReps]);

  const allDRepsIndex = useMemo(
    () => new Map(allDReps.map((e) => [e.drepId, e])),
    [allDReps]
  );

  // The default cohort is a random twenty, so a favourite is usually absent
  // from it. Pinning them above keeps them reachable without a mode switch,
  // and excluding any that are already on screen avoids showing a card twice.
  const isDefaultCohortView = !isFavoritesView && !isShowAll && !isSearchActive;
  // Favourites pin above the directory wherever the directory is being
  // browsed, whether that is the suggested cohort or every DRep. Pinning them
  // only over the cohort, and only when they were not already in it, meant the
  // same favourite moved or vanished depending on which control was last
  // pressed. A search is the one exception: results there are matches for what
  // was typed, not a place to reintroduce entries that did not match.
  const showPinnedFavorites = !isFavoritesView && !isSearchActive;

  const pinnedFavorites = useMemo(() => {
    if (!showPinnedFavorites) return [];
    return favoriteEntries;
  }, [showPinnedFavorites, favoriteEntries]);

  const pinnedFavoriteIds = useMemo(
    () => new Set(pinnedFavorites.map((entry) => entry.drepId)),
    [pinnedFavorites]
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
    // Whatever is pinned above is not repeated here, so a favourite appears
    // once and always in the same place.
    const withoutPinned = base.filter(
      (entry) => !pinnedFavoriteIds.has(entry.drepId)
    );
    const filtered = filterDReps(withoutPinned, filters);
    // Sorting is a property of a list, not of how wide the list is: twenty
    // suggestions can be read in an order too. Search results keep the
    // relevance order the query produced.
    if (isSearchActive) return filtered;
    // The default is not a plain shuffle. A thousand DReps in ledger order
    // asks a reader to assess each from scratch, so they arrive in bands of
    // what delegating would achieve, randomised inside each band so that no
    // stable position at the top of one is worth holding.
    if (sort === 'default') {
      return orderDRepsByStanding(filtered, totalDRepStake, orderSeed);
    }
    return sortDReps(filtered, sort);
  }, [
    suggestedDReps,
    pinnedFavoriteIds,
    filters,
    isSearchActive,
    totalDRepStake,
    orderSeed,
    isShowAll,
    queryKind,
    searchIndex,
    searchQuery,
    allDReps,
    sort,
  ]);

  const favoritesEntries = useMemo(
    () => allDReps.filter((entry) => favoriteDRepIds.has(entry.drepId)),
    [favoriteDRepIds, allDReps]
  );

  // Favourites are searched among themselves. A user with a long list wants to
  // find one of their own, not to be handed the whole directory again.
  const favoritesSearchIndex = useMemo(
    () => buildDRepSearchIndex(favoritesEntries),
    [favoritesEntries]
  );

  const visibleFavorites = useMemo(() => {
    if (!isSearchActive) return favoritesEntries;
    return queryKind === 'name'
      ? searchDRepsByName(favoritesEntries, searchQuery)
      : searchDRepsByIdPrefix(favoritesSearchIndex, searchQuery);
  }, [
    favoritesEntries,
    favoritesSearchIndex,
    isSearchActive,
    queryKind,
    searchQuery,
  ]);

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

  // Mirrors the stake pools screen, which swaps its list label for a search
  // variant rather than leaving a stale one above the results.
  const resolveMainListTitle = () => {
    if (isSearchActive) return messages.listTitleSearch;
    if (isShowAll) return messages.listTitleAll;
    return messages.listTitleSuggested;
  };
  const mainListTitleMessage = resolveMainListTitle();

  // Narrowed, not merely widened. Showing every DRep changes neither the
  // ordering nor the membership criteria, so it does not belong here: with
  // randomized still selected the order is still randomized, and saying
  // otherwise stated something untrue about the list on screen. The ordering
  // caveat has its own disclosure, shown when the sort actually changes.
  const isFilteredView = isSearchActive || !isDefaultFilterState(filters);

  const handleShowAllChange = (nextShowAll: boolean) => {
    setIsShowAll(nextShowAll);
    if (!nextShowAll) setSort('default');
  };

  const handleClearFilters = () => {
    setSearchQuery('');
    setFilters(DEFAULT_DREP_FILTER_STATE);
    setSort('default');
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

  // Search and the view toggle share a row, as they do on the stake pools
  // screen. The favourites view renders the same row: a long favourites list
  // needs finding and rearranging just as much as the directory does.
  const renderSearchRow = () => (
    <div className={styles.searchRow}>
      <DRepDirectorySearch
        value={searchQuery}
        queryKind={queryKind}
        onChange={setSearchQuery}
      />
      <div className={styles.viewButtons} role="group">
        <button
          type="button"
          className={viewMode === 'cards' ? styles.selected : undefined}
          aria-pressed={viewMode === 'cards'}
          aria-label={intl.formatMessage(messages.viewModeCards)}
          title={intl.formatMessage(messages.viewModeCards)}
          onClick={() => setViewMode('cards')}
        >
          <SVGInline svg={gridIcon} />
        </button>
        <button
          type="button"
          className={viewMode === 'table' ? styles.selected : undefined}
          aria-pressed={viewMode === 'table'}
          aria-label={intl.formatMessage(messages.viewModeTable)}
          title={intl.formatMessage(messages.viewModeTable)}
          onClick={() => setViewMode('table')}
        >
          <SVGInline svg={listIcon} />
        </button>
      </div>
    </div>
  );

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
          if (favoritesEntries.length === 0) {
            return (
              <DRepEmptyState
                variant="noFavorites"
                onBackToDirectory={onBackToDirectory}
              />
            );
          }
          return (
            <>
              {renderSearchRow()}
              {visibleFavorites.length === 0 ? (
                <DRepEmptyState
                  variant="noResults"
                  onClearFilters={handleClearFilters}
                />
              ) : (
                <DRepDirectoryList
                  entries={visibleFavorites}
                  viewMode={viewMode}
                  favoriteDRepIds={favoriteDRepIds}
                  onToggleFavorite={onToggleFavorite}
                  isFavoritesView
                  isStaleFavoriteEntry={isStaleFavoriteEntry}
                  isCurrentDRep={isCurrentDRep}
                  canDelegate={canDelegate}
                  totalDRepStake={totalDRepStake}
                  epochLength={epochLength}
                  slotLength={slotLength}
                  onSelectForDelegation={onSelectForDelegation}
                  onViewDetails={onViewDetails}
                />
              )}
            </>
          );
        }
        return (
          <>
            {renderSearchRow()}
            {showErrorBanner && (
              <DRepErrorBanner
                variant="refreshFailed"
                retryLabel={intl.formatMessage(messages.retry)}
                onRetry={onRefresh}
                lastFetchedAt={lastFetchedAt}
              />
            )}
            {/* The bias the randomized order guards against does not depend
                on how wide the list is, so neither does the disclosure. */}
            {!isSearchActive && sort === 'votingPowerDesc' && (
              <div className={styles.sortBiasWarning} role="status">
                {intl.formatMessage(messages.sortBiasWarning)}
              </div>
            )}
            {/* The criteria belong to the suggested cohort alone: nothing is
                being suggested while every DRep is listed or a query is being
                answered, so there is nothing here to state or to adjust. */}
            {/* Stating what the list is does not depend on being able to
                change it, so the summary is not gated on the handler the
                controls need. Gating both together meant a caller that only
                displays the directory lost the explanation as well. */}
            {/* One box, in one place, whichever mode is selected. The mode
                buttons, the sentence saying what is on screen and the controls
                that change it all live together, so switching modes swaps what
                the box says rather than making a row of controls appear above
                a box that disappears. */}
            <div className={styles.criteriaSummary}>
              {/* One row, whichever mode is selected: the mode control leads
                  it, and what follows is either the suggestion criteria or the
                  filters and ordering that replace them. Two rows in one mode
                  and one in the other made the same box look like two
                  different screens. */}
              <div className={styles.controls}>
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
                {/* Open, not behind a disclosure. The controls name the
                    criteria and show their state at once, so hiding them hid
                    the explanation of the list as well as the means of
                    changing it. */}
                {isDefaultCohortView && onCohortCriteriaChange && (
                  <DRepCohortCriteriaPanel
                    criteria={cohortCriteria}
                    onCriteriaChange={onCohortCriteriaChange}
                  />
                )}
              </div>
              {/* Last line of the box in either mode, so the count sits in one
                  place and only its wording follows what is being counted. A
                  figure that moves when the mode changes is a figure a reader
                  has to find again. */}
              {visibleEntries.length > 0 && (
                <p className={styles.criteriaSummaryLead}>
                  {isShowAll || isSearchActive
                    ? intl.formatMessage(messages.showAllSummary, {
                        count: visibleEntries.length,
                        total: allDReps.length,
                      })
                    : intl.formatMessage(messages.criteriaSummary, {
                        count: visibleEntries.length,
                      })}
                </p>
              )}
            </div>
            {/* A cohort assembled under criteria other than the ones the
                panel shows says so. Silently loosening them would leave the
                controls describing a list they did not produce. */}
            {isDefaultCohortView && relaxedCohortCriteria.length > 0 && (
              <div className={styles.relaxedNotice} role="status">
                {intl.formatMessage(messages.relaxedNotice, {
                  criteria: relaxedCohortCriteria
                    .map((criterion) =>
                      intl.formatMessage(criterionMessages[criterion])
                    )
                    .join(', '),
                })}
              </div>
            )}
            {pinnedFavorites.length > 0 && (
              <section
                className={styles.pinnedFavorites}
                aria-label={intl.formatMessage(messages.pinnedFavoritesTitle, {
                  count: pinnedFavorites.length,
                })}
              >
                <h2 className={styles.pinnedFavoritesTitle}>
                  {intl.formatMessage(messages.pinnedFavoritesTitle, {
                    count: pinnedFavorites.length,
                  })}
                </h2>
                <DRepDirectoryList
                  entries={pinnedFavorites}
                  favoriteDRepIds={favoriteDRepIds}
                  onToggleFavorite={onToggleFavorite}
                  isCurrentDRep={isCurrentDRep}
                  isStaleFavoriteEntry={isStaleFavoriteEntry}
                  canDelegate={canDelegate}
                  totalDRepStake={totalDRepStake}
                  epochLength={epochLength}
                  slotLength={slotLength}
                  onSelectForDelegation={onSelectForDelegation}
                  onViewDetails={onViewDetails}
                />
              </section>
            )}
            {visibleEntries.length === 0 ? (
              <DRepEmptyState
                variant={
                  // Nothing was asked for, so nothing was excluded by asking:
                  // the network simply holds no DRep the criteria admit.
                  isDefaultCohortView && isDefaultFilterState(filters)
                    ? 'noSuggestions'
                    : 'noResults'
                }
                onClearFilters={handleClearFilters}
                onShowAll={handleShowAllFromEmptyState}
              />
            ) : (
              <>
                {/* Both lists carry a label, as they do on the stake pools
                    screen. Labelling only the favorites left the list beneath
                    it looking like a continuation of them. */}
                <h2 className={styles.listTitle}>
                  {intl.formatMessage(mainListTitleMessage)}{' '}
                  {intl.formatMessage(messages.listTitleCount, {
                    count: visibleEntries.length,
                  })}
                </h2>
                <DRepDirectoryList
                  entries={visibleEntries}
                  viewMode={viewMode}
                  favoriteDRepIds={favoriteDRepIds}
                  onToggleFavorite={onToggleFavorite}
                  isCurrentDRep={isCurrentDRep}
                  canDelegate={canDelegate}
                  totalDRepStake={totalDRepStake}
                  epochLength={epochLength}
                  slotLength={slotLength}
                  onSelectForDelegation={onSelectForDelegation}
                  onViewDetails={onViewDetails}
                />
              </>
            )}
            {/* One control for the width of the list, at the point a reader
                reaches the end of what is shown, rather than a checkbox among
                the filters that had to be found first. */}
            {!isSearchActive && !isFavoritesView && canDelegate && (
              <div className={styles.rerollFooter}>
                {isShowAll ? (
                  <Button
                    className="flat"
                    label={intl.formatMessage(messages.backToSuggestions)}
                    onClick={() => handleShowAllChange(false)}
                    skin={ButtonSkin}
                  />
                ) : (
                  <>
                    <Button
                      label={intl.formatMessage(messages.reroll)}
                      onClick={onReroll}
                      disabled={
                        refreshState === GovernanceRefreshState.Loading ||
                        refreshState === GovernanceRefreshState.Refreshing
                      }
                      skin={ButtonSkin}
                    />
                    <Button
                      className="flat"
                      label={intl.formatMessage(messages.showAllCta)}
                      onClick={() => handleShowAllChange(true)}
                      skin={ButtonSkin}
                    />
                  </>
                )}
              </div>
            )}
            {/* Abstain and No Confidence are standing options rather than
                properties of any particular list, so nothing a user does to
                the list takes them away: not showing every DRep, not
                searching. They sit below the results under their own heading,
                where they read as the alternatives they are rather than as
                matches for a query.

                Both take the flat treatment. Neither is what the directory is
                for, so neither is a primary action; and the two are not
                separated by weight either, because the difference between
                them is one of meaning rather than of consequence. Their
                descriptions carry that difference. */}
            {canDelegate && (
              <div className={styles.suggestedFooter}>
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
                      className="flat"
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
                      className="flat"
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
