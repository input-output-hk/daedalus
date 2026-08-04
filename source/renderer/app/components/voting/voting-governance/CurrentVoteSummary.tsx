import React from 'react';
import { injectIntl, intlShape } from 'react-intl';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepSourceLabel from '../../governance/_shared/DRepSourceLabel';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';
import type { WalletVotingTarget } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { messages } from './CurrentVoteSummary.messages';
import styles from './CurrentVoteSummary.scss';

type Props = {
  currentVote: WalletVotingTarget | null;
  drepEntry?: AppDRepDirectoryEntry | null;
  intl: intlShape.isRequired;
};

const EXPIRING_MAX_REMAINING_EPOCHS = 12;

type CurrentVoteBadgeState = 'unavailable' | 'inactive' | 'expiring' | 'active';

// This panel shows the wallet's own delegation, which is not cohort-scoped, so
// the expiring window is the full remaining-epoch threshold rather than the
// narrower window the directory badge applies to cohort members.
function deriveCurrentVoteBadgeState(
  drepEntry: AppDRepDirectoryEntry | null | undefined
): CurrentVoteBadgeState {
  if (drepEntry == null) return 'unavailable';
  if (drepEntry.status === 'inactive') return 'inactive';
  if (
    drepEntry.drepActivity != null &&
    drepEntry.drepActivity <= EXPIRING_MAX_REMAINING_EPOCHS
  ) {
    return 'expiring';
  }
  return 'active';
}

// The vote-kind chip and the status captions render through the local message
// set because DRepSourceLabel's variant union cannot express them;
// DRepSourceLabel renders only the on-chain source label on the DRep state.
function CurrentVoteSummary({ currentVote, drepEntry, intl }: Props) {
  if (currentVote == null) {
    return null;
  }

  if (currentVote.kind === 'drep') {
    const badgeState = deriveCurrentVoteBadgeState(drepEntry);
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        <div className={styles.statusRow}>
          <span className={styles.statusBadge}>
            <span className={styles.glyph} aria-hidden="true">
              ●
            </span>
            {intl.formatMessage(messages.statusDelegatedToDRep)}
          </span>
          <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
          {(badgeState === 'active' || badgeState === 'inactive') && (
            <DRepStatusBadge status={drepEntry.status} />
          )}
          {badgeState === 'expiring' && (
            <span className={styles.expiringBadge}>
              <span className={styles.glyph} aria-hidden="true">
                ▲
              </span>
              {intl.formatMessage(messages.statusExpiringBadge, {
                n: drepEntry.drepActivity,
              })}
            </span>
          )}
        </div>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentVote.drep.raw} />
        </div>
        {badgeState !== 'active' && (
          <p className={styles.caption}>
            {badgeState === 'expiring'
              ? intl.formatMessage(messages.statusExpiring, {
                  n: drepEntry.drepActivity,
                })
              : intl.formatMessage(
                  badgeState === 'inactive'
                    ? messages.statusInactive
                    : messages.statusUnavailable
                )}
          </p>
        )}
      </section>
    );
  }

  const isAbstain = currentVote.kind === 'abstain';
  return (
    <section
      className={styles.component}
      aria-label={intl.formatMessage(messages.headerCurrent)}
    >
      <h3 className={styles.header}>
        {intl.formatMessage(messages.headerCurrent)}
      </h3>
      <div className={styles.statusRow}>
        <span className={styles.statusBadge}>
          <span className={styles.glyph} aria-hidden="true">
            {isAbstain ? '⊘' : '✕'}
          </span>
          {intl.formatMessage(
            isAbstain ? messages.statusAbstain : messages.statusNoConfidence
          )}
        </span>
      </div>
      <p className={styles.caption}>
        {intl.formatMessage(
          isAbstain ? messages.abstainCaption : messages.noConfidenceCaption
        )}
      </p>
    </section>
  );
}

export default injectIntl(CurrentVoteSummary);
