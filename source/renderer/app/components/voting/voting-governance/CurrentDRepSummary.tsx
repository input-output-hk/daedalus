import React from 'react';
import { injectIntl, intlShape } from 'react-intl';
import { isLapsingSoon } from '../../governance/_shared/drepExpiry';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { messages } from './CurrentDRepSummary.messages';
import styles from './CurrentVoteSummary.scss';

type Props = {
  currentDRep: DRepDelegation | null;
  drepEntry?: AppDRepDirectoryEntry | null;
  intl: intlShape.isRequired;
};

// One definition of approaching expiry across the app: the same six-epoch
// threshold the DRep directory badge and its filter use. This panel had kept
// its own twelve-epoch window, which is sixty of a DRep's hundred days.

type CurrentVoteBadgeState = 'unavailable' | 'inactive' | 'expiring' | 'active';

// This panel shows the wallet's own delegation, which is not cohort-scoped, so
// the expiring window is the full remaining-epoch threshold rather than the
// narrower window the directory badge applies to cohort members.
function deriveCurrentVoteBadgeState(
  drepEntry: AppDRepDirectoryEntry | null | undefined
): CurrentVoteBadgeState {
  if (drepEntry == null) return 'unavailable';
  if (drepEntry.status === 'inactive') return 'inactive';
  if (isLapsingSoon(drepEntry.drepActivity)) {
    return 'expiring';
  }
  return 'active';
}

// The vote-kind chip and the status captions render through the local message
// set because DRepSourceLabel's variant union cannot express them;
// DRepSourceLabel renders only the on-chain source label on the DRep state.
function CurrentDRepSummary({ currentDRep, drepEntry, intl }: Props) {
  if (currentDRep == null) {
    return null;
  }

  if (currentDRep.kind === 'drep') {
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
          {/* One badge with three states, the same component the directory
              and the voting centre use. A bespoke expiring badge beside a
              status badge was a second implementation of the thing that
              exists to stop those two contradicting each other. */}
          {drepEntry && (
            <DRepStatusBadge
              status={drepEntry.status}
              drepActivity={drepEntry.drepActivity}
            />
          )}
        </div>
        {/* The name was fetched and then never shown, so a delegation a user
            had chosen by name read back to them as a truncated identifier. */}
        {drepEntry?.verifiedName && (
          <p className={styles.drepName}>{drepEntry.verifiedName}</p>
        )}
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentDRep.drep.raw} />
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

  const isAbstain = currentDRep.kind === 'abstain';
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

export default injectIntl(CurrentDRepSummary);
