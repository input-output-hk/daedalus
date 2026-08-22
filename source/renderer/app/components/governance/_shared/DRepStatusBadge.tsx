import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import { epochsToDays, isLapsingSoon } from './drepExpiry';
import type { DRepStatus } from '../../../../../common/types/governance.types';
import styles from './DRepStatusBadge.scss';

const messages = defineMessages({
  active: {
    id: 'governance.drepDirectory.status.active',
    defaultMessage: '!!!Active',
    description: 'DRep active status badge label',
  },
  inactive: {
    id: 'governance.drepDirectory.status.inactive',
    defaultMessage: '!!!Inactive',
    description: 'DRep inactive status badge label',
  },
  expiring: {
    id: 'governance.drepDirectory.expiry.label',
    defaultMessage: '!!!Expiring soon',
    description:
      'Badge shown when a DRep is close to losing its voting power through inactivity',
  },
  expiringDetailWithDays: {
    id: 'governance.drepDirectory.expiry.detailWithDays',
    defaultMessage:
      '!!!Voting power lapses in {epochs, plural, one {# epoch} other {# epochs}} ({days, plural, one {# day} other {# days}}) unless this DRep records activity by voting, updating its metadata or re-registering.',
    description:
      'Explains when a DRep loses its voting power, in epochs and days',
  },
  expiringDetail: {
    id: 'governance.drepDirectory.expiry.detail',
    defaultMessage:
      '!!!Voting power lapses in {epochs, plural, one {# epoch} other {# epochs}} unless this DRep records activity by voting, updating its metadata or re-registering.',
    description:
      'Explains when a DRep loses its voting power when the epoch length is unknown',
  },
});

/**
 * A DRep's standing, as one badge rather than several.
 *
 * The three states are ordered, not independent: a DRep has to be active to be
 * expiring, and one that has already lapsed is inactive rather than both. Two
 * badges side by side let those combinations be rendered, and the pairings
 * that resulted ("Active" beside "Expiring soon", or worse "Inactive" beside
 * it) either restated one another or contradicted each other outright.
 */
export type DRepStanding = 'active' | 'expiring' | 'inactive';

export function getDRepStanding(
  status: DRepStatus,
  drepActivity: number | null | undefined
): DRepStanding {
  if (status !== 'active') return 'inactive';
  return isLapsingSoon(drepActivity) ? 'expiring' : 'active';
}

interface Props {
  status: DRepStatus;
  drepActivity?: number | null;
  epochLength?: number | null;
  slotLength?: number | null;
  intl: intlShape.isRequired;
}

function DRepStatusBadge({
  status,
  drepActivity = null,
  epochLength = null,
  slotLength = null,
  intl,
}: Props) {
  const standing = getDRepStanding(status, drepActivity);
  const label = intl.formatMessage(messages[standing]);

  // Only the expiring state has anything more to say: how long is left, and
  // what a DRep can do about it.
  let detail: string | undefined;
  if (standing === 'expiring') {
    const epochs = drepActivity as number;
    const days = epochsToDays(epochs, epochLength, slotLength);
    detail =
      days != null
        ? intl.formatMessage(messages.expiringDetailWithDays, { epochs, days })
        : intl.formatMessage(messages.expiringDetail, { epochs });
  }

  return (
    <span
      className={classNames(styles.badge, styles[standing])}
      title={detail}
      aria-label={detail ? `${label}. ${detail}` : label}
    >
      <span className={styles.label}>{label}</span>
    </span>
  );
}

export default injectIntl(DRepStatusBadge);
