import React from 'react';
import { injectIntl, intlShape } from 'react-intl';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { messages } from './CurrentDRepSummary.messages';
import { sharedGovernanceMessages } from './shared-messages';
import styles from './CurrentVoteSummary.scss';

type Props = {
  currentDRep: DRepDelegation | null;
  drepEntry?: AppDRepDirectoryEntry | null;
  // Whether the lookup for this DRep is still outstanding. Without it, a DRep
  // that has retired and one whose data has not arrived yet are the same
  // absence, and the panel called both of them loading: a wallet delegated to
  // a retired DRep said "loading" for as long as it was open.
  isLookingUpDRep?: boolean;
  intl: intlShape.isRequired;
};

// Whether the DRep's directory entry arrived, and nothing more. Classifying
// how close that DRep is to inactive belongs to the badge, which does it for
// the directory and the detail view too; deriving it a second time here is
// how the two screens came to disagree about the same DRep on the same day.

type CurrentVoteLookupState = 'loading' | 'unavailable' | 'resolved';

function deriveLookupState(
  drepEntry: AppDRepDirectoryEntry | null | undefined,
  isLookingUpDRep: boolean
): CurrentVoteLookupState {
  if (drepEntry != null) return 'resolved';
  return isLookingUpDRep ? 'loading' : 'unavailable';
}

function CurrentDRepSummary({
  currentDRep,
  drepEntry,
  isLookingUpDRep = false,
  intl,
}: Props) {
  if (currentDRep == null) {
    return null;
  }

  if (currentDRep.kind === 'drep') {
    const lookupState = deriveLookupState(drepEntry, isLookingUpDRep);
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        {/* One badge with three states, the same component the directory
            uses. A bespoke badge counting down beside a status badge was a
            second implementation of the thing that exists to stop those two
            contradicting each other.

            No "Delegated to DRep" chip beside it: the heading above already
            says this wallet is delegated, and the name and id below say to
            what, so the chip stated the same fact a third time. */}
        {drepEntry && (
          <div className={styles.statusRow}>
            <DRepStatusBadge
              status={drepEntry.status}
              drepActivity={drepEntry.drepActivity}
            />
          </div>
        )}
        {/* The name was fetched and then never shown, so a delegation a user
            had chosen by name read back to them as a truncated identifier. */}
        {drepEntry?.verifiedName && (
          <p className={styles.drepName}>{drepEntry.verifiedName}</p>
        )}
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentDRep.drep.raw} />
        </div>
        {/* Nothing said here about a DRep that is inactive or close to it.
            The badge above states it, and this panel exists on the screen for
            changing a delegation, so a sentence telling the reader to consider
            redelegating tells them to do what they are already doing. What is
            left is the case the badge cannot show, because there is no entry
            to draw one from. */}
        {lookupState !== 'resolved' && (
          <p className={styles.caption}>
            {intl.formatMessage(
              lookupState === 'loading'
                ? messages.statusLoading
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
            isAbstain
              ? sharedGovernanceMessages.abstain
              : sharedGovernanceMessages.noConfidence
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
