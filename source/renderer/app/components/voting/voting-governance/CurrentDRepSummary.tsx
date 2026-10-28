import React from 'react';
import { FormattedMessage, injectIntl, intlShape } from 'react-intl';
import type BigNumber from 'bignumber.js';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepStatusBadge from '../../governance/_shared/DRepStatusBadge';
import { DiscreetWalletAmount } from '../../../features/discreet-mode';
import type {
  DRepLookup,
  VoteDelegationState,
} from '../../../utils/governance/voteDelegationState';
import { messages } from './CurrentDRepSummary.messages';
import { sharedGovernanceMessages } from './shared-messages';
import styles from './CurrentVoteSummary.scss';
import { governanceSharedMessages } from '../../governance/_shared/governanceSharedMessages';

type Props = {
  state: VoteDelegationState;
  // The lookup of the DRep the state names. A lookup that has not answered is
  // loading; one that answered not found or failed says so, so a DRep that
  // retired and one whose data has not arrived are never the same absence.
  lookup?: DRepLookup;
  // The reward balance the ledger holds back while there is no delegation.
  heldReward?: BigNumber | null;
  intl: intlShape.isRequired;
};

function CurrentDRepSummary({
  state,
  lookup = { status: 'idle' },
  heldReward = null,
  intl,
}: Props) {
  if (state.kind === 'none') {
    return null;
  }

  if (state.kind === 'ended' || state.kind === 'not_in_registry') {
    const ended = state.kind === 'ended';
    const hasHeldReward = ended && heldReward != null && heldReward.gt(0);
    let text = messages.notInRegistry;
    if (ended) {
      text = state.registeredAgain
        ? messages.endedRetiredRegisteredAgain
        : messages.endedRetired;
    }
    const header = ended ? messages.headerEnded : messages.headerCurrent;
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(header)}
      >
        <h3 className={styles.header}>{intl.formatMessage(header)}</h3>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={state.drep.raw} />
        </div>
        <p className={styles.caption}>{intl.formatMessage(text)}</p>
        {hasHeldReward && (
          <p className={styles.caption}>
            <FormattedMessage
              {...messages.heldRewards}
              values={{
                rewardsAmount: <DiscreetWalletAmount amount={heldReward} />,
              }}
            />
          </p>
        )}
      </section>
    );
  }

  const currentDRep = state.voting;
  const drepEntry = lookup.status === 'found' ? lookup.entry : null;

  if (currentDRep.kind === 'drep') {
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
        {drepEntry == null && (
          <p className={styles.caption}>
            {intl.formatMessage(
              lookup.status === 'idle' || lookup.status === 'pending'
                ? governanceSharedMessages.loadingDRepData
                : messages.lookupFailed
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
          isAbstain
            ? governanceSharedMessages.abstainDescription
            : governanceSharedMessages.noConfidenceDescription
        )}
      </p>
    </section>
  );
}

export default injectIntl(CurrentDRepSummary);
