import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
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
    description: 'Toggle that switches from the default cohort to all DReps',
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
  expiryLabel: {
    id: 'governance.drepDirectory.filter.expiry',
    defaultMessage: '!!!Expiry',
    description: 'Label of the expiry-threshold filter',
  },
  optionAll: {
    id: 'governance.drepDirectory.filter.all',
    defaultMessage: '!!!All',
    description: 'Neutral option of every filter dropdown',
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
    defaultMessage: '!!!With metadata',
    description: 'Metadata filter option: anchor present',
  },
  metadataWithout: {
    id: 'governance.drepDirectory.filter.metadata.without',
    defaultMessage: '!!!Without metadata',
    description: 'Metadata filter option: no anchor',
  },
  expiryThresholdWindow: {
    id: 'governance.drepDirectory.filter.expiry.thresholdWindow',
    defaultMessage: '!!!Expiring in 7–12 epochs',
    description: 'Expiry filter option: the threshold window',
  },
  excludeTop35: {
    id: 'governance.drepDirectory.filter.excludeTop35',
    defaultMessage: '!!!Exclude the 35 largest',
    description: 'Show-all filter that removes the top-35 by voting power',
  },
  favorited: {
    id: 'governance.drepDirectory.filter.favorited',
    defaultMessage: '!!!Favorited',
    description: 'Filter that shows only favorited DReps',
  },
  sortLabel: {
    id: 'governance.drepDirectory.sort.label',
    defaultMessage: '!!!Sort',
    description: 'Label of the show-all sort dropdown',
  },
  sortRandomized: {
    id: 'governance.drepDirectory.sort.randomized',
    defaultMessage: '!!!Randomized (default)',
    description: 'Default seeded-random sort option',
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
  return (
    <div className={styles.container}>
      <Checkbox
        className={styles.toggle}
        label={intl.formatMessage(messages.showAll)}
        checked={isShowAll}
        onChange={onShowAllChange}
        skin={CheckboxSkin}
      />
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.statusLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.statusLabel)}
        value={filters.status}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            status: event.target.value as DRepStatusFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="active">
          {intl.formatMessage(messages.statusActive)}
        </option>
        <option value="inactive">
          {intl.formatMessage(messages.statusInactive)}
        </option>
      </select>
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.metadataLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.metadataLabel)}
        value={filters.metadata}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            metadata: event.target.value as DRepMetadataFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="withMetadata">
          {intl.formatMessage(messages.metadataWith)}
        </option>
        <option value="withoutMetadata">
          {intl.formatMessage(messages.metadataWithout)}
        </option>
      </select>
      <span className={styles.filterLabel}>
        {intl.formatMessage(messages.expiryLabel)}
      </span>
      <select
        className={styles.select}
        aria-label={intl.formatMessage(messages.expiryLabel)}
        value={filters.expiry}
        onChange={(event) =>
          onFiltersChange({
            ...filters,
            expiry: event.target.value as DRepExpiryFilter,
          })
        }
      >
        <option value="all">{intl.formatMessage(messages.optionAll)}</option>
        <option value="thresholdWindow">
          {intl.formatMessage(messages.expiryThresholdWindow)}
        </option>
      </select>
      <Checkbox
        className={styles.toggle}
        label={intl.formatMessage(messages.favorited)}
        checked={filters.favoritedOnly}
        onChange={(checked: boolean) =>
          onFiltersChange({ ...filters, favoritedOnly: checked })
        }
        skin={CheckboxSkin}
      />
      {isShowAll && isRankingAvailable && (
        <Checkbox
          className={styles.toggle}
          label={intl.formatMessage(messages.excludeTop35)}
          checked={filters.excludeTop35}
          onChange={(checked: boolean) =>
            onFiltersChange({ ...filters, excludeTop35: checked })
          }
          skin={CheckboxSkin}
        />
      )}
      {isShowAll && !isSearchActive && (
        <>
          <span className={styles.filterLabel}>
            {intl.formatMessage(messages.sortLabel)}
          </span>
          <select
            className={styles.select}
            aria-label={intl.formatMessage(messages.sortLabel)}
            value={sort}
            onChange={(event) =>
              onSortChange(event.target.value as DRepSortOption)
            }
          >
            <option value="randomized">
              {intl.formatMessage(messages.sortRandomized)}
            </option>
            {isRankingAvailable && (
              <option value="votingPowerDesc">
                {intl.formatMessage(messages.sortVotingPowerDesc)}
              </option>
            )}
            {isRankingAvailable && (
              <option value="votingPowerAsc">
                {intl.formatMessage(messages.sortVotingPowerAsc)}
              </option>
            )}
            <option value="expiryAsc">
              {intl.formatMessage(messages.sortExpiryAsc)}
            </option>
          </select>
        </>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryFilters);
