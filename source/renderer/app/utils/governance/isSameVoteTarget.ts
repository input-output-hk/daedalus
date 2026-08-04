import type { WalletVotingTarget } from '../../api/wallets/types';
import { normalizeDRepIdentity } from './normalizeDRepIdentity';

/**
 * Same-vote comparison keyed on the case-stable (credentialHex, credentialType)
 * pair: bech32 letter case is not meaningful, and an absent credentialHex never
 * establishes equality because credentialType alone cannot identify a credential.
 */
export function isSameVoteTarget(
  chosenOption: string,
  currentVote: WalletVotingTarget | null
): boolean {
  if (currentVote == null) return false;
  if (chosenOption === 'abstain') return currentVote.kind === 'abstain';
  if (chosenOption === 'no_confidence') {
    return currentVote.kind === 'no_confidence';
  }
  if (currentVote.kind !== 'drep') return false;

  const selected = normalizeDRepIdentity(chosenOption);
  if (selected == null) return false;
  if (
    selected.credentialHex == null ||
    currentVote.drep.credentialHex == null
  ) {
    return false;
  }
  return (
    selected.credentialHex.toLowerCase() ===
      currentVote.drep.credentialHex.toLowerCase() &&
    selected.credentialType === currentVote.drep.credentialType
  );
}
