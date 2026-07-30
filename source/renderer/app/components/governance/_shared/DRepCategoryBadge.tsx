import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import type {
  AppDRepDirectoryEntry,
  DRepCohortContext,
} from '../../../stores/GovernanceStore';
import styles from './DRepCategoryBadge.scss';

const messages = defineMessages({
  highValue: {
    id: 'governance.drepDirectory.category.highValue',
    defaultMessage: '!!!High value',
    description: 'Category badge for in-cohort DReps above the cohort median',
  },
  highValueTooltip: {
    id: 'governance.drepDirectory.category.highValue.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view, with verified metadata and voting power above the cohort median.',
    description: 'Tooltip explaining the High value category',
  },
  primary: {
    id: 'governance.drepDirectory.category.primary',
    defaultMessage: '!!!Primary',
    description: 'Category badge for DReps with anchor metadata',
  },
  primaryTooltip: {
    id: 'governance.drepDirectory.category.primary.tooltip',
    defaultMessage: '!!!Has verified off-chain metadata.',
    description: 'Tooltip explaining the Primary category',
  },
  threshold: {
    id: 'governance.drepDirectory.category.threshold',
    defaultMessage: '!!!Threshold',
    description: 'Category badge for DReps in the 7-12 epoch expiry window',
  },
  thresholdTooltip: {
    id: 'governance.drepDirectory.category.threshold.tooltip',
    defaultMessage: '!!!Approaching expiry — review before delegating.',
    description: 'Tooltip explaining the Threshold category',
  },
  nonMetadata: {
    id: 'governance.drepDirectory.category.nonMetadata',
    defaultMessage: '!!!Non-metadata',
    description: 'Category badge for DReps without anchor metadata',
  },
  nonMetadataTooltip: {
    id: 'governance.drepDirectory.category.nonMetadata.tooltip',
    defaultMessage:
      '!!!Eligible for delegation but has no verified off-chain metadata yet.',
    description: 'Tooltip explaining the Non-metadata category',
  },
});

export type DRepCategory =
  | 'highValue'
  | 'threshold'
  | 'primary'
  | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'drepId' | 'votingPower' | 'drepActivity'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

/**
 * Priority is High value > Threshold > Primary > Non-metadata. Cohort
 * membership and verified metadata are explicit inputs, never re-derived
 * here, and High value cannot render outside the cohort where the median
 * is undefined. Informational only - never read back by ordering,
 * filtering or cohort code.
 */
export function getDRepCategory(
  entry: DRepCategorySource,
  cohort: DRepCohortContext
): DRepCategory {
  const inCohort = cohort.memberIds?.has(entry.drepId) ?? false;
  const hasVerifiedMetadata = cohort.verifiedMetadataIds.has(entry.drepId);
  const isAboveMedian =
    inCohort &&
    entry.votingPower != null &&
    cohort.medianVotingPower != null &&
    entry.votingPower.isGreaterThan(cohort.medianVotingPower);

  if (inCohort && hasVerifiedMetadata && isAboveMedian) {
    return 'highValue';
  }
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return hasVerifiedMetadata ? 'primary' : 'nonMetadata';
}

interface Props {
  entry: DRepCategorySource;
  cohort: DRepCohortContext;
  intl: intlShape.isRequired;
}

function DRepCategoryBadge({ entry, cohort, intl }: Props) {
  const category = getDRepCategory(entry, cohort);
  const labelByCategory: Record<DRepCategory, string> = {
    highValue: intl.formatMessage(messages.highValue),
    nonMetadata: intl.formatMessage(messages.nonMetadata),
    primary: intl.formatMessage(messages.primary),
    threshold: intl.formatMessage(messages.threshold),
  };
  const tooltipByCategory: Record<DRepCategory, string> = {
    highValue: intl.formatMessage(messages.highValueTooltip),
    nonMetadata: intl.formatMessage(messages.nonMetadataTooltip),
    primary: intl.formatMessage(messages.primaryTooltip),
    threshold: intl.formatMessage(messages.thresholdTooltip),
  };

  return (
    <span
      className={classNames(styles.badge, styles[category])}
      title={tooltipByCategory[category]}
      aria-label={`${labelByCategory[category]}. ${tooltipByCategory[category]}`}
    >
      <span className={styles.dot} aria-hidden="true" />
      <span className={styles.label}>{labelByCategory[category]}</span>
    </span>
  );
}

export default injectIntl(DRepCategoryBadge);
