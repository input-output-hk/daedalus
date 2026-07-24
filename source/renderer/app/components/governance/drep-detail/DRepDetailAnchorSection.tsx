import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type { DRepAnchorPresence } from '../../../../../common/types/governance.types';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchor.title',
    defaultMessage: '!!!Anchor',
    description: 'Heading of the anchor section on the DRep detail view',
  },
  urlLabel: {
    id: 'governance.drepDetail.anchor.url',
    defaultMessage: '!!!Anchor URL',
    description: 'Label for the on-chain anchor URL field',
  },
  hashLabel: {
    id: 'governance.drepDetail.anchor.hash',
    defaultMessage: '!!!Anchor hash',
    description: 'Label for the on-chain anchor hash field',
  },
  sourceRowLabel: {
    id: 'governance.drepDetail.anchor.source',
    defaultMessage: '!!!Source',
    description: 'Label for the anchor source-label row',
  },
  none: {
    id: 'governance.drepDetail.anchor.none',
    defaultMessage: '!!!No anchor is recorded on-chain for this DRep.',
    description: 'Shown when the DRep registered without an anchor',
  },
});

interface Props {
  anchor: DRepAnchorPresence | null;
  intl: intlShape.isRequired;
}

function DRepDetailAnchorSection({ anchor, intl }: Props) {
  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <h2 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h2>
      {anchor ? (
        <dl className={styles.fieldList}>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.urlLabel)}
            </dt>
            {/* Deliberately inert text: no anchor may be fetched, rendered as
                a link, or opened before the hardened anchor pipeline lands. */}
            <dd className={styles.anchorValue}>{anchor.url}</dd>
          </div>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.hashLabel)}
            </dt>
            <dd className={styles.anchorValue}>{anchor.hash}</dd>
          </div>
          <div className={styles.fieldRow}>
            <dt className={styles.fieldLabel}>
              {intl.formatMessage(messages.sourceRowLabel)}
            </dt>
            <dd className={styles.fieldValue}>
              <DRepSourceLabel
                source="on-chain-anchor-reference"
                className={styles.sourceLabel}
              />
            </dd>
          </div>
        </dl>
      ) : (
        <p className={styles.mutedValue}>{intl.formatMessage(messages.none)}</p>
      )}
    </section>
  );
}

export default injectIntl(DRepDetailAnchorSection);
