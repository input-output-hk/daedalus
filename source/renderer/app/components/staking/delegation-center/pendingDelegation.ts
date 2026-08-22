import { get } from 'lodash';
import {
  carriesStakeTarget,
  WalletDelegationStatuses,
} from '../../../domains/Wallet';
import type { WalletPendingDelegations } from '../../../api/wallets/types';

/**
 * The stake pool a wallet will be delegating to in `epochNumber`, or
 * `fallbackStakePoolId` when nothing pending changes it.
 *
 * A vote delegation pending in the same epoch says nothing about the stake
 * pool, so it must neither stand in for the stake entry nor hide it: reading
 * whichever entry happened to match the epoch first reported "not delegating"
 * for a wallet whose stake delegation was untouched.
 */
export function getPendingStakePoolIdForEpoch(
  pendingDelegations: WalletPendingDelegations | null | undefined,
  epochNumber: number,
  fallbackStakePoolId: string | null | undefined
): string | null | undefined {
  if (!pendingDelegations || !pendingDelegations.length) {
    return fallbackStakePoolId;
  }

  const stakeChange = pendingDelegations
    .filter((delegation) => carriesStakeTarget(get(delegation, 'status', null)))
    .find(
      (delegation) =>
        get(delegation, ['changes_at', 'epoch_number'], 0) === epochNumber
    );

  if (!stakeChange) {
    return fallbackStakePoolId;
  }

  // A pending undelegation is a real answer: no pool from this epoch on.
  if (get(stakeChange, 'status') === WalletDelegationStatuses.NOT_DELEGATING) {
    return null;
  }

  return get(stakeChange, 'target', null);
}
