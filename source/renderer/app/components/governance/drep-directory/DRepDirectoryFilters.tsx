import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import globalMessages from '../../../i18n/global-messages';
import DRepFacetSelect from '../_shared/DRepFacetSelect';
import type {
  DRepFilterState,
  DRepMetadataFilter,
  DRepSortOption,
  DRepStatusFilter,
} from './helpers';
import styles from './DRepDirectoryFilters.scss';

const messages = defineMessages({
  modeSuggested: {
    id: 'governance.drepDirectory.mode.suggested',
    defaultMessage: '!!!Suggested',
    description: 'Mode showing a suggested cohort drawn under criteria',
  },
  modeAll: {
    id: 'governance.drepDirectory.mode.all',
    defaultMessage: '!!!All DReps',
    description: 'Mode showing every DRep, with filters and ordering',
  },
  modeGroupLabel: {
    id: 'governance.drepDirectory.mode.groupLabel',
    defaultMessage: '!!!What the directory shows',
    description: 'Accessible name of the pair of mode buttons',
  },
  showAll: {
    id: 'governance.drepDirectory.cohortBanner.showAll',
    defaultMessage: '!!!Show all DReps',
    description: 'Filter-bar toggle between the cohort and every DRep',
  },
  statusLabel: {
    id: 'governance.drepDirectory.filter.active',
    defaultMessage: '!!!Status',
    description: 'Label of the status filter',
  },
  metadataLabel: {
    id: 'governance.drepDirectory.filter.metadata',
    defaultMessage: '!!!Metadata',
    description: 'Label of the metadata filter',
  },
  statusActive: {
    id: 'governance.drepDirectory.status.active',
    defaultMessage: '!!!Active',
    description: 'Active status label',
  },
  statusInactiveSoon: {
    id: 'governance.drepDirectory.status.inactiveSoon',
    defaultMessage: '!!!Inactive Soon',
    description: 'Status filter option for DReps close to going inactive',
  },
  statusInactive: {
    id: 'governance.drepDirectory.status.inactive',
    defaultMessage: '!!!Inactive',
    description: 'Inactive status label',
  },
  metadataWith: {
    id: 'governance.drepDirectory.filter.metadata.with',
    defaultMessage: '!!!With verified metadata',
    description: 'Metadata filter option: anchor present',
  },
  metadataWithout: {
    id: 'governance.drepDirectory.filter.metadata.without',
    defaultMessage: '!!!Without verified metadata',
    description: 'Metadata filter option: no anchor',
  },
  sortLabel: {
    id: 'governance.drepDirectory.sort.label',
    defaultMessage: '!!!Sort',
    description: 'Label of the show-all sort dropdown',
  },
  sortDefault: {
    id: 'governance.drepDirectory.sort.default',
    defaultMessage: '!!!Default',
    description:
      'Default ordering: grouped by what delegating would achieve, random within each group. Named for its role rather than its mechanism, and kept distinct from the Suggested mode',
  },
  sortVotingPowerDesc: {
    id: 'governance.drepDirectory.sort.votingPowerDesc',
    defaultMessage: '!!!Voting power (high to low)',
    description: 'Voting power descending sort option',
  },
  sortVotingPowerAsc: {
    id: 'governance.drepDirectory.sort.votingPowerAsc',
    defaultMessage: '!!!Voting power (low to high)',
    description: 'Voting power ascending sort option',
  },
});

interface Props {
  filters: DRepFilterState;
  onFiltersChange: (filters: DRepFilterState) => void;
  isShowAll: boolean;
  onShowAllChange: (isShowAll: boolean) => void;
  sort: DRepSortOption;
  onSortChange: (sort: DRepSortOption) => void;
  isRankingAvailable: boolean;
  isSearchActive: boolean;
  intl: intlShape.isRequired;
}

function DRepDirectoryFilters({
  filters,
  onFiltersChange,
  isShowAll,
  onShowAllChange,
  sort,
  onSortChange,
  isRankingAvailable,
  isSearchActive,
  intl,
}: Props) {
  const canFilterPopulation = isShowAll || isSearchActive;
  const modeGroupLabel = intl.formatMessage(messages.modeGroupLabel);
  const facet = (
    label: string,
    value: string,
    onChange: (next: string) => void,
    options: Array<[string, string]>
  ) => (
    <DRepFacetSelect
      key={label}
      label={label}
      value={value}
      onChange={onChange}
      options={options}
    />
  );

  return (
    <div className={styles.container}>
      {/* Two named things rather than one thing switched on: a suggested
          twenty and every DRep there is. Built as the pair of pressed buttons
          the view toggle beside the search already uses, because a switch
          reads as on and off and neither of these is off. */}
      <div className={styles.modes} role="group" aria-label={modeGroupLabel}>
        <button
          type="button"
          className={!isShowAll ? styles.modeSelected : undefined}
          aria-pressed={!isShowAll}
          onClick={() => onShowAllChange(false)}
        >
          {intl.formatMessage(messages.modeSuggested)}
        </button>
        <button
          type="button"
          className={isShowAll ? styles.modeSelected : undefined}
          aria-pressed={isShowAll}
          onClick={() => onShowAllChange(true)}
        >
          {intl.formatMessage(messages.modeAll)}
        </button>
      </div>
      {/* Both facets narrow on properties the suggestion criteria already
          decided, so over the cohort they are inert at best: picking Inactive
          there can only ever return nothing, which reads as a broken screen
          rather than as a filter that had no work to do. They appear where
          they have something to narrow, which is every DRep and a search over
          all of them. The states themselves are the badge's own, so the filter
          and the card cannot disagree about one DRep. */}
      {canFilterPopulation &&
        facet(
          intl.formatMessage(messages.statusLabel),
          filters.status,
          (next) =>
            onFiltersChange({ ...filters, status: next as DRepStatusFilter }),
          [
            ['all', intl.formatMessage(globalMessages.all)],
            ['active', intl.formatMessage(messages.statusActive)],
            ['inactiveSoon', intl.formatMessage(messages.statusInactiveSoon)],
            ['inactive', intl.formatMessage(messages.statusInactive)],
          ]
        )}
      {canFilterPopulation &&
        facet(
          intl.formatMessage(messages.metadataLabel),
          filters.metadata,
          (next) =>
            onFiltersChange({
              ...filters,
              metadata: next as DRepMetadataFilter,
            }),
          [
            ['all', intl.formatMessage(globalMessages.all)],
            ['withMetadata', intl.formatMessage(messages.metadataWith)],
            ['withoutMetadata', intl.formatMessage(messages.metadataWithout)],
          ]
        )}
      {/* Ordering belongs to the list of every DRep. Over the suggested cohort
          it had nowhere useful to go: the criteria already exclude anything
          above the ceiling, so a power sort could only reorder twenty entries
          that all sit below it, and offering it there meant two of the three
          options silently switched the reader to a different mode. A search
          returns matches in relevance order and is not re-sorted at all. */}
      {isShowAll &&
        !isSearchActive &&
        facet(
          intl.formatMessage(messages.sortLabel),
          sort,
          (next) => onSortChange(next as DRepSortOption),
          [
            ['default', intl.formatMessage(messages.sortDefault)],
            ...(isRankingAvailable
              ? ([
                  [
                    'votingPowerDesc',
                    intl.formatMessage(messages.sortVotingPowerDesc),
                  ],
                  [
                    'votingPowerAsc',
                    intl.formatMessage(messages.sortVotingPowerAsc),
                  ],
                ] as Array<[string, string]>)
              : []),
          ]
        )}
    </div>
  );
}

export default injectIntl(DRepDirectoryFilters);
