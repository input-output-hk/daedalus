import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import globalMessages from '../../../i18n/global-messages';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import DRepFacetSelect from '../_shared/DRepFacetSelect';
import type {
  DRepExpiryFilter,
  DRepFilterState,
  DRepMetadataFilter,
  DRepSortOption,
  DRepStatusFilter,
} from './helpers';
import styles from './DRepDirectoryFilters.scss';

const messages = defineMessages({
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
  expiryLabel: {
    id: 'governance.drepDirectory.filter.expiry',
    defaultMessage: '!!!Expiry',
    description: 'Label of the expiry filter',
  },
  expiryHideLapsing: {
    id: 'governance.drepDirectory.filter.expiry.hideLapsingSoon',
    defaultMessage: '!!!Hide DReps lapsing within 6 epochs',
    description:
      'Filter option that excludes DReps whose voting power lapses soon',
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
  sortRecommended: {
    id: 'governance.drepDirectory.sort.recommended',
    defaultMessage: '!!!Recommended (default)',
    description:
      'Default sort: bands by what delegating would achieve, random within each',
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
  sortExpiryAsc: {
    id: 'governance.drepDirectory.sort.expiryAsc',
    defaultMessage: '!!!Expiry (soonest first)',
    description: 'Soonest-expiry-first sort option',
  },
  sortExpiryDesc: {
    id: 'governance.drepDirectory.sort.expiryDesc',
    defaultMessage: '!!!Expiry (soonest last)',
    description: 'Latest-expiry-first sort option',
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
      {/* Widening the pool governs every facet beside it, so it leads the
          strip. The same state the footer buttons drive: kept here as well
          because this is where someone used to this screen will look. */}
      <div className={styles.switchFacet}>
        <span className={styles.switchLabel}>
          {intl.formatMessage(messages.showAll)}
        </span>
        <div className={styles.switchControl}>
          <NormalSwitch
            className={styles.showAllSwitch}
            checked={isShowAll}
            onChange={onShowAllChange}
          />
        </div>
      </div>
      {facet(
        intl.formatMessage(messages.statusLabel),
        filters.status,
        (next) =>
          onFiltersChange({ ...filters, status: next as DRepStatusFilter }),
        [
          ['all', intl.formatMessage(globalMessages.all)],
          ['active', intl.formatMessage(messages.statusActive)],
          ['inactive', intl.formatMessage(messages.statusInactive)],
        ]
      )}
      {facet(
        intl.formatMessage(messages.metadataLabel),
        filters.metadata,
        (next) =>
          onFiltersChange({ ...filters, metadata: next as DRepMetadataFilter }),
        [
          ['all', intl.formatMessage(globalMessages.all)],
          ['withMetadata', intl.formatMessage(messages.metadataWith)],
          ['withoutMetadata', intl.formatMessage(messages.metadataWithout)],
        ]
      )}
      {facet(
        intl.formatMessage(messages.expiryLabel),
        filters.expiry,
        (next) =>
          onFiltersChange({ ...filters, expiry: next as DRepExpiryFilter }),
        [
          ['all', intl.formatMessage(globalMessages.all)],
          ['hideLapsingSoon', intl.formatMessage(messages.expiryHideLapsing)],
        ]
      )}
      {!isSearchActive &&
        facet(
          intl.formatMessage(messages.sortLabel),
          sort,
          (next) => onSortChange(next as DRepSortOption),
          [
            ['recommended', intl.formatMessage(messages.sortRecommended)],
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
            ['expiryAsc', intl.formatMessage(messages.sortExpiryAsc)],
            ['expiryDesc', intl.formatMessage(messages.sortExpiryDesc)],
          ]
        )}
    </div>
  );
}

export default injectIntl(DRepDirectoryFilters);
