import React from 'react';
import { intlShape } from 'react-intl';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import {
  formatSnapshotDate,
  formatSnapshotSize,
  truncateDigest,
} from './snapshotFormatting';
import styles from './MithrilSnapshotDetails.scss';

interface Props {
  selectedSnapshot?: MithrilSnapshotItem | null;
}

interface Context {
  intl: Intl;
}

function MithrilSnapshotDetails(props: Props, { intl }: Context) {
  const { selectedSnapshot } = props;

  if (!selectedSnapshot) {
    return (
      <div className={styles.details}>
        <div className={styles.detailsHeader}>
          {intl.formatMessage(messages.snapshotDetailsTitle)}
        </div>
        <div className={styles.detailsFallback}>
          {intl.formatMessage(messages.snapshotDetailsUnavailable)}
        </div>
      </div>
    );
  }

  const createdAt = formatSnapshotDate(selectedSnapshot.createdAt, intl.locale);
  const size = formatSnapshotSize(selectedSnapshot.size);
  const digest = truncateDigest(selectedSnapshot.digest);

  return (
    <div className={styles.details}>
      <div className={styles.detailsHeader}>
        {intl.formatMessage(messages.snapshotDetailsTitle)}
      </div>
      <div className={styles.detailsGrid}>
        <div className={styles.detailRow}>
          <div className={styles.detailLabel}>
            {intl.formatMessage(messages.snapshotDigestLabel)}
          </div>
          <div
            className={`${styles.detailValue} ${styles.detailValueDigest}`}
            title={selectedSnapshot.digest}
          >
            {digest}
          </div>
        </div>
        <div className={styles.detailRow}>
          <div className={styles.detailLabel}>
            {intl.formatMessage(messages.snapshotCreatedLabel)}
          </div>
          <div className={styles.detailValue}>{createdAt || 'n/a'}</div>
        </div>
        <div className={styles.detailRow}>
          <div className={styles.detailLabel}>
            {intl.formatMessage(messages.snapshotSizeLabel)}
          </div>
          <div className={styles.detailValue}>{size || 'n/a'}</div>
        </div>
        <div className={styles.detailRow}>
          <div className={styles.detailLabel}>
            {intl.formatMessage(messages.snapshotNodeVersionLabel)}
          </div>
          <div className={styles.detailValue}>
            {selectedSnapshot.cardanoNodeVersion || 'n/a'}
          </div>
        </div>
      </div>
    </div>
  );
}

MithrilSnapshotDetails.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilSnapshotDetails;
