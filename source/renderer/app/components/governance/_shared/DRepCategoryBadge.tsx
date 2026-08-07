import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepCategoryBadge.scss';

const messages = defineMessages({
  primary: {
    id: 'governance.drepDirectory.category.primary',
    defaultMessage: '!!!Primary',
    description: 'Category badge for DReps with a verified name',
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
    description: 'Category badge for DReps without a verified name',
  },
  nonMetadataTooltip: {
    id: 'governance.drepDirectory.category.nonMetadata.tooltip',
    defaultMessage:
      '!!!Eligible for delegation but has no verified off-chain metadata yet.',
    description: 'Tooltip explaining the Non-metadata category',
  },
});

export type DRepCategory = 'threshold' | 'primary' | 'nonMetadata';

export type DRepCategorySource = Pick<
  AppDRepDirectoryEntry,
  'verifiedName' | 'drepActivity'
>;

const THRESHOLD_WINDOW_MIN = 7;
const THRESHOLD_WINDOW_MAX = 12;

export function getDRepCategory(entry: DRepCategorySource): DRepCategory {
  if (
    entry.drepActivity != null &&
    entry.drepActivity >= THRESHOLD_WINDOW_MIN &&
    entry.drepActivity <= THRESHOLD_WINDOW_MAX
  ) {
    return 'threshold';
  }
  return entry.verifiedName != null ? 'primary' : 'nonMetadata';
}

interface Props {
  entry: DRepCategorySource;
  intl: intlShape.isRequired;
}

function DRepCategoryBadge({ entry, intl }: Props) {
  const category = getDRepCategory(entry);
  const labelByCategory: Record<DRepCategory, string> = {
    nonMetadata: intl.formatMessage(messages.nonMetadata),
    primary: intl.formatMessage(messages.primary),
    threshold: intl.formatMessage(messages.threshold),
  };
  const tooltipByCategory: Record<DRepCategory, string> = {
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
