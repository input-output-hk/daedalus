import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepCategoryBadge.scss';

const messages = defineMessages({
  primary: {
    id: 'governance.drepDirectory.category.primary',
    defaultMessage: '!!!Primary',
    description: 'Category badge for DReps with anchor metadata',
  },
  primaryTooltip: {
    id: 'governance.drepDirectory.category.primary.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view with verified metadata.',
    description: 'Tooltip explaining the Primary category',
  },
  threshold: {
    id: 'governance.drepDirectory.category.threshold',
    defaultMessage: '!!!Threshold',
    description: 'Category badge for DReps in the 7-12 epoch expiry window',
  },
  thresholdTooltip: {
    id: 'governance.drepDirectory.category.threshold.tooltip',
    defaultMessage:
      '!!!Inside the default Recommended view but approaching expiry — review before delegating.',
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

export type DRepCategory = 'primary' | 'threshold' | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'status' | 'drepActivity' | 'anchor'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

/**
 * Category rules with binding priority Threshold > Primary > Non-metadata:
 * the 7-12 remaining-epoch window wins outright; otherwise on-chain anchor
 * presence is the interim metadata-completeness proxy until the verified
 * anchor pipeline exists. Informational only - never used to order or
 * filter the cohort.
 */
export function getDRepCategory(entry: DRepCategorySource): DRepCategory {
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return entry.anchor != null ? 'primary' : 'nonMetadata';
}

interface Props {
  entry: DRepCategorySource;
  intl: intlShape.isRequired;
}

function DRepCategoryBadge({ entry, intl }: Props) {
  const category = getDRepCategory(entry);
  const labelByCategory: Record<DRepCategory, string> = {
    primary: intl.formatMessage(messages.primary),
    threshold: intl.formatMessage(messages.threshold),
    nonMetadata: intl.formatMessage(messages.nonMetadata),
  };
  const tooltipByCategory: Record<DRepCategory, string> = {
    primary: intl.formatMessage(messages.primaryTooltip),
    threshold: intl.formatMessage(messages.thresholdTooltip),
    nonMetadata: intl.formatMessage(messages.nonMetadataTooltip),
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
