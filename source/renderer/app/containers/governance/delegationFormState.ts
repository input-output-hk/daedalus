import type { VoteType } from '../../components/voting/voting-governance/types';

/**
 * Shape of the delegation-form handoff carried in React Router
 * location.state. This is the only transport for the DRep-ID handoff —
 * never query params, never store-backed pending form state.
 */
export interface DelegationFormNavigationState {
  from?: string;
  selectedWalletId?: string | null;
  voteType?: VoteType;
  selectedDRepId?: string;
  selectedDRepVerifiedName?: string | null;
  selectedDRepAnchorUrl?: string | null;
}

const VOTE_TYPES: ReadonlyArray<VoteType> = [
  'abstain',
  'no_confidence',
  'drep',
];

// location.state is untyped at the router boundary; pick only the handoff
// fields so unrelated router state can never reach the form contract.
export function pickDelegationFormNavigationState(
  state: unknown
): DelegationFormNavigationState | undefined {
  if (!state || typeof state !== 'object') return undefined;
  const candidate = state as DelegationFormNavigationState;
  const picked: DelegationFormNavigationState = {};
  if (typeof candidate.from === 'string') {
    picked.from = candidate.from;
  }
  if (
    typeof candidate.selectedWalletId === 'string' ||
    candidate.selectedWalletId === null
  ) {
    picked.selectedWalletId = candidate.selectedWalletId;
  }
  if (VOTE_TYPES.includes(candidate.voteType as VoteType)) {
    picked.voteType = candidate.voteType;
  }
  if (typeof candidate.selectedDRepId === 'string') {
    picked.selectedDRepId = candidate.selectedDRepId;
  }
  if (
    typeof candidate.selectedDRepVerifiedName === 'string' ||
    candidate.selectedDRepVerifiedName === null
  ) {
    picked.selectedDRepVerifiedName = candidate.selectedDRepVerifiedName;
  }
  if (
    typeof candidate.selectedDRepAnchorUrl === 'string' ||
    candidate.selectedDRepAnchorUrl === null
  ) {
    picked.selectedDRepAnchorUrl = candidate.selectedDRepAnchorUrl;
  }
  return Object.keys(picked).length > 0 ? picked : undefined;
}

// Directory-side forwarding contract: any push toward a detail path forwards
// only { from, selectedWalletId, voteType }. The slice-4 "View details" CTA
// will call this; until then the Jest harness exercises it.
export function pickDelegationFormReturnState(
  state: unknown
): DelegationFormNavigationState | undefined {
  const picked = pickDelegationFormNavigationState(state);
  if (!picked) return undefined;
  const returnState: DelegationFormNavigationState = {};
  if (picked.from !== undefined) returnState.from = picked.from;
  if (picked.selectedWalletId !== undefined) {
    returnState.selectedWalletId = picked.selectedWalletId;
  }
  if (picked.voteType !== undefined) returnState.voteType = picked.voteType;
  return Object.keys(returnState).length > 0 ? returnState : undefined;
}
