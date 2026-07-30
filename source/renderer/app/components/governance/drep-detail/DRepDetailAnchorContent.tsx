import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type { AnchorEnrichEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.anchorContent.title',
    defaultMessage: '!!!Off-chain profile',
    description: 'Heading of the verified anchor content block',
  },
  givenName: {
    id: 'governance.drepDetail.anchorContent.givenName',
    defaultMessage: '!!!Name',
    description: 'Label for the verified CIP-119 givenName field',
  },
  loading: {
    id: 'governance.drepDetail.anchorContent.loading',
    defaultMessage: '!!!Checking the anchor…',
    description: 'Shown while the anchor is being fetched and verified',
  },
  unavailable: {
    id: 'governance.drepDetail.anchorContent.unavailable',
    defaultMessage:
      '!!!The off-chain profile could not be verified. Only on-chain data is shown.',
    description: 'Shown when the anchor fetch or hash verification failed',
  },
  caption: {
    id: 'governance.drepDetail.anchorContent.caption',
    defaultMessage:
      "!!!This name is the DRep's own claim, hash-matched to the anchor recorded on-chain. Daedalus does not verify identity.",
    description:
      'Caption stating that a verified name is not verified identity',
  },
});

interface Props {
  state: AnchorEnrichEntry | null;
  intl: intlShape.isRequired;
}

function DRepDetailAnchorContent({ state, intl }: Props) {
  if (!state) return null;

  if (state.state === 'loading') {
    return (
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.loading)}
      </p>
    );
  }

  if (state.state === 'unavailable') {
    return (
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.unavailable)}{' '}
        <DRepSourceLabel
          source="anchor-unavailable"
          className={styles.sourceLabel}
        />
      </p>
    );
  }

  if (state.givenName == null) return null;

  return (
    <>
      <h3 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h3>
      <dl className={styles.fieldList}>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.givenName)}
          </dt>
          <dd className={styles.fieldValue}>
            {state.givenName}{' '}
            <DRepSourceLabel
              source="verified-off-chain"
              host={state.host}
              className={styles.sourceLabel}
            />
          </dd>
        </div>
      </dl>
      <p className={styles.mutedValue}>
        {intl.formatMessage(messages.caption)}
      </p>
    </>
  );
}

export default injectIntl(DRepDetailAnchorContent);
