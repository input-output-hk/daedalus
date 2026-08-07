import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.onchain.title',
    defaultMessage: '!!!On-chain',
    description: 'Heading of the on-chain section on the DRep detail view',
  },
  sourceRowLabel: {
    id: 'governance.drepDetail.onchain.source',
    defaultMessage: '!!!Source',
    description: 'Label for the on-chain section source-label row',
  },
  statusLabel: {
    id: 'governance.drepDetail.status',
    defaultMessage: '!!!Status',
    description: 'Label for the DRep status field on the detail view',
  },
  expiresInLabel: {
    id: 'governance.drepDetail.expiresIn',
    defaultMessage: '!!!Expires in',
    description: 'Label for the remaining-epochs field on the detail view',
  },
  expiresInValue: {
    id: 'governance.drepDetail.expiresInEpochs',
    defaultMessage: '!!!{count} epochs',
    description: 'Remaining epochs until the DRep expires',
  },
  votingPowerLabel: {
    id: 'governance.drepDetail.votingPower',
    defaultMessage: '!!!Voting power',
    description: 'Label for the voting power field on the detail view',
  },
  votingPowerLovelace: {
    id: 'governance.drepDetail.votingPowerLovelace',
    defaultMessage: '!!!({amount} lovelace)',
    description: 'Secondary raw-lovelace line under the ADA voting power',
  },
  votingPowerLoadingTooltip: {
    id: 'governance.drepDetail.votingPower.loadingTooltip',
    defaultMessage: '!!!Loading voting power…',
    description: 'Tooltip on the voting-power placeholder during enrichment',
  },
  votingPowerUnavailableTooltip: {
    id: 'governance.drepDetail.votingPower.unavailableTooltip',
    defaultMessage: '!!!Stake distribution unavailable this refresh.',
    description: 'Tooltip on the voting-power placeholder when stake failed',
  },
  votePositionsLabel: {
    id: 'governance.drepDetail.votePositions',
    defaultMessage: '!!!Current votes',
    description: 'Label for the current-epoch vote positions field',
  },
  votePositionsUnavailable: {
    id: 'governance.drepDetail.votePositions.unavailable',
    defaultMessage: '!!!Vote positions are not available in this version.',
    description: 'Graceful value when vote positions cannot be shown',
  },
});

interface Props {
  entry: AppDRepDetail;
  intl: intlShape.isRequired;
}

// Detail-form rendering: full ADA with thousands separators; the raw
// lovelace renders on a secondary line, never rounded away.
function formatAdaExact(lovelace: BigNumber): string {
  return `₳ ${lovelace.div(1_000_000).toFormat()}`;
}

function DRepDetailOnchainSection({ entry, intl }: Props) {
  const votingPowerTooltip =
    entry.votingPower === null
      ? intl.formatMessage(messages.votingPowerUnavailableTooltip)
      : undefined;

  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <h2 className={styles.sectionTitle}>
        {intl.formatMessage(messages.title)}
      </h2>
      <dl className={styles.fieldList}>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.statusLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            <DRepStatusBadge status={entry.status} />
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.expiresInLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.status === 'active' && entry.drepActivity != null
              ? intl.formatMessage(messages.expiresInValue, {
                  count: entry.drepActivity,
                })
              : '—'}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votingPowerLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.votingPower ? (
              <>
                <span className={styles.votingPowerAda}>
                  {formatAdaExact(entry.votingPower)}
                </span>
                <span className={styles.votingPowerLovelace}>
                  {intl.formatMessage(messages.votingPowerLovelace, {
                    amount: entry.votingPower.toFormat(0),
                  })}
                </span>
              </>
            ) : (
              <span title={votingPowerTooltip} aria-label={votingPowerTooltip}>
                —
              </span>
            )}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votePositionsLabel)}
          </dt>
          <dd className={styles.mutedValue}>
            {intl.formatMessage(messages.votePositionsUnavailable)}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.sourceRowLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
          </dd>
        </div>
      </dl>
    </section>
  );
}

export default injectIntl(DRepDetailOnchainSection);
