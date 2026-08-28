import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import { epochsToDays, getDRepStanding } from './drepExpiry';
import type { DRepStanding } from './drepExpiry';
import type { DRepStatus } from '../../../../../common/types/governance.types';
import { governanceSharedMessages } from './governanceSharedMessages';
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
  expiringDetailWithDays: {
    id: 'governance.drepDirectory.expiry.detailWithDays',
    defaultMessage:
      '!!!Will become inactive in {epochs, plural, one {# epoch} other {# epochs}} ({days, plural, one {# day} other {# days}}) without on-chain activity.',
    description:
      'Explains when a DRep loses its voting power, in epochs and days',
  },
  expiringDetail: {
    id: 'governance.drepDirectory.expiry.detail',
    defaultMessage:
      '!!!Will become inactive in {epochs, plural, one {# epoch} other {# epochs}} without on-chain activity.',
    description:
      'Explains when a DRep loses its voting power when the epoch length is unknown',
  },
});

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
  // Resolved by hand rather than by indexing the message set: one of the
  // three now lives in the shared set, and an index that misses simply hands
  // formatMessage undefined.
  const label = intl.formatMessage(
    standing === 'inactiveSoon'
      ? governanceSharedMessages.inactiveSoon
      : messages[standing]
  );

  // Only the expiring state has anything more to say: how long is left, and
  // what a DRep can do about it.
  let detail: string | undefined;
  if (standing === 'inactiveSoon') {
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
