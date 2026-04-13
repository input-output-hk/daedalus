import React from 'react';
import { intlShape } from 'react-intl';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_SNAPSHOT_DETAILS_HEADING_ID } from './accessibilityIds';
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
      <section
        className={styles.details}
        aria-labelledby={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID}
      >
        <h2
          className={styles.detailsHeader}
          id={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID}
          tabIndex={-1}
        >
          {intl.formatMessage(messages.snapshotDetailsTitle)}
        </h2>
        <p className={styles.detailsFallback}>
          {intl.formatMessage(messages.snapshotDetailsUnavailable)}
        </p>
      </section>
    );
  }

  const createdAt = formatSnapshotDate(selectedSnapshot.createdAt, intl.locale);
  const size = formatSnapshotSize(selectedSnapshot.size);
  const digest = truncateDigest(selectedSnapshot.digest);

  return (
    <section
      className={styles.details}
      aria-labelledby={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID}
    >
      <h2
        className={styles.detailsHeader}
        id={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID}
        tabIndex={-1}
      >
        {intl.formatMessage(messages.snapshotDetailsTitle)}
      </h2>
      <dl className={styles.detailsList}>
        <div className={styles.detailGroup}>
          <dt className={styles.detailTerm}>
            {intl.formatMessage(messages.snapshotDigestLabel)}
          </dt>
          <dd
            className={`${styles.detailDescription} ${styles.detailValueDigest}`}
            title={selectedSnapshot.digest}
          >
            {digest}
          </dd>
        </div>
        <div className={styles.detailGroup}>
          <dt className={styles.detailTerm}>
            {intl.formatMessage(messages.snapshotCreatedLabel)}
          </dt>
          <dd className={styles.detailDescription}>{createdAt || 'n/a'}</dd>
        </div>
        <div className={styles.detailGroup}>
          <dt className={styles.detailTerm}>
            {intl.formatMessage(messages.snapshotSizeLabel)}
          </dt>
          <dd className={styles.detailDescription}>{size || 'n/a'}</dd>
        </div>
        <div className={styles.detailGroup}>
          <dt className={styles.detailTerm}>
            {intl.formatMessage(messages.snapshotNodeVersionLabel)}
          </dt>
          <dd className={styles.detailDescription}>
            {selectedSnapshot.cardanoNodeVersion || 'n/a'}
          </dd>
        </div>
      </dl>
    </section>
  );
}

MithrilSnapshotDetails.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilSnapshotDetails;
