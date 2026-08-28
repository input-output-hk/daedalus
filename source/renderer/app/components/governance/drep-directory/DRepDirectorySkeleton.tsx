import React from 'react';
import classNames from 'classnames';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepDirectorySkeleton.scss';
import { governanceSharedMessages } from '../_shared/governanceSharedMessages';

const messages = defineMessages({});

// Mirrors the directory list page size so the first paint holds the height the
// loaded page will occupy.
const SKELETON_CARD_COUNT = 25;

interface Props {
  count?: number;
  intl: intlShape.isRequired;
}

function DRepDirectorySkeleton({ count = SKELETON_CARD_COUNT, intl }: Props) {
  return (
    <div
      className={styles.skeletonList}
      role="status"
      aria-busy="true"
      aria-label={intl.formatMessage(governanceSharedMessages.loadingDRepData)}
    >
      {Array.from({ length: count }, (_, index) => (
        <div className={styles.skeletonCard} key={index} aria-hidden="true">
          <div className={styles.topRow}>
            <span className={classNames(styles.block, styles.badge)} />
            <span className={classNames(styles.block, styles.badge)} />
            <span className={classNames(styles.block, styles.id)} />
          </div>
          <div className={styles.bottomRow}>
            <span className={classNames(styles.block, styles.label)} />
            <span className={classNames(styles.block, styles.value)} />
          </div>
          <div className={styles.actionsRow}>
            <span className={classNames(styles.block, styles.action)} />
            <span className={classNames(styles.block, styles.action)} />
          </div>
        </div>
      ))}
    </div>
  );
}

export default injectIntl(DRepDirectorySkeleton);
