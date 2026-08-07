import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
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
});

interface Props {
  status: DRepStatus;
  intl: intlShape.isRequired;
}

function DRepStatusBadge({ status, intl }: Props) {
  const labelMap: Record<DRepStatus, string> = {
    active: intl.formatMessage(messages.active),
    inactive: intl.formatMessage(messages.inactive),
  };

  return (
    <span
      className={classNames(styles.badge, styles[status])}
      aria-label={labelMap[status]}
    >
      <span className={styles.dot} aria-hidden="true" />
      <span className={styles.label}>{labelMap[status]}</span>
    </span>
  );
}

export default injectIntl(DRepStatusBadge);
