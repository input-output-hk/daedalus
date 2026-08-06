import type { DRepDelegation } from '../../api/wallets/types';
import { normalizeDRepIdentity } from './normalizeDRepIdentity';

/**
 * Same-vote comparison keyed on the case-stable (credentialHex, credentialType)
 * pair: bech32 letter case is not meaningful, and an absent credentialHex never
 * establishes equality because credentialType alone cannot identify a credential.
 */
export function isSameDRep(
  chosenOption: string,
  currentDRep: DRepDelegation | null
): boolean {
  if (currentDRep == null) return false;
  if (chosenOption === 'abstain') return currentDRep.kind === 'abstain';
  if (chosenOption === 'no_confidence') {
    return currentDRep.kind === 'no_confidence';
  }
  if (currentDRep.kind !== 'drep') return false;

  const selected = normalizeDRepIdentity(chosenOption);
  if (selected == null) return false;
  if (
    selected.credentialHex == null ||
    currentDRep.drep.credentialHex == null
  ) {
    return false;
  }
  return (
    selected.credentialHex.toLowerCase() ===
      currentDRep.drep.credentialHex.toLowerCase() &&
    selected.credentialType === currentDRep.drep.credentialType
  );
}
