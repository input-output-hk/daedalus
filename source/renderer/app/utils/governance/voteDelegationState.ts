import type { DRepDelegation } from '../../api/wallets/types';
import type { DRepIdentity } from '../../../../common/types/governance.types';
import type Wallet from '../../domains/Wallet';
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';

export type DRepLookup =
  | { status: 'idle' | 'pending' | 'not_found' | 'failed' }
  | { status: 'found'; entry: AppDRepDirectoryEntry };

export type VoteDelegationState =
  | { kind: 'none' }
  | { kind: 'current'; voting: DRepDelegation; source: 'ledger' | 'history' }
  | { kind: 'ended'; drep: DRepIdentity; registeredAgain: boolean }
  | { kind: 'not_in_registry'; drep: DRepIdentity };

type WalletVoteFields = Pick<
  Wallet,
  'isLegacy' | 'votingTarget' | 'ledgerVote'
>;

/** The key a DRep's lookups are held under: its CIP-129 identifier. */
export const drepLookupKey = (drep: DRepIdentity): string =>
  drep.cip129 ?? drep.raw;

/**
 * What a wallet's voting power is delegated to, from the ledger's delegation
 * and the wallet's own history together.
 *
 * Where the wallet reports the ledger's delegation it is the current vote, and
 * the history only names the DRep of a delegation that ended: the ledger
 * clears an account's vote delegation when its DRep retires. Without that
 * field the history is all there is, and a DRep the wallet's registry answers
 * not found for is reported as missing. The registry decides only the wording
 * of an ended delegation, never whether it ended: a DRep that has registered
 * again does not get its earlier delegations back.
 */
export const voteDelegationState = (
  wallet: WalletVoteFields,
  lookup: (drepId: string) => DRepLookup
): VoteDelegationState => {
  if (wallet.isLegacy) return { kind: 'none' };
  // A wallet read before the field existed carries none.
  const ledger = wallet.ledgerVote ?? { status: 'unknown' };
  const history = wallet.votingTarget ?? null;
  if (ledger.status === 'delegated') {
    return { kind: 'current', voting: ledger.voting, source: 'ledger' };
  }
  if (ledger.status === 'not_delegated') {
    if (history?.kind !== 'drep') return { kind: 'none' };
    return {
      kind: 'ended',
      drep: history.drep,
      registeredAgain: lookup(drepLookupKey(history.drep)).status === 'found',
    };
  }
  if (history == null) return { kind: 'none' };
  if (
    history.kind === 'drep' &&
    lookup(drepLookupKey(history.drep)).status === 'not_found'
  ) {
    return { kind: 'not_in_registry', drep: history.drep };
  }
  return { kind: 'current', voting: history, source: 'history' };
};

/** The vote a new delegation is compared against: the current one, if any. */
export const effectiveCurrentVote = (
  state: VoteDelegationState
): DRepDelegation | null => (state.kind === 'current' ? state.voting : null);

/** The DRep a state names: the one delegated to, or the one an ended delegation named. */
export const drepOfState = (
  state: VoteDelegationState
): DRepIdentity | null => {
  if (state.kind === 'ended' || state.kind === 'not_in_registry') {
    return state.drep;
  }
  if (state.kind === 'current' && state.voting.kind === 'drep') {
    return state.voting.drep;
  }
  return null;
};
