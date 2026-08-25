/**
 * A DRep's voting power lapses unless it records activity within `dRepActivity`
 * epochs, which is 20 on both mainnet and preprod. Activity means registering,
 * voting, or updating its metadata: any of the three refreshes the counter, so
 * a DRep close to expiry has not necessarily failed to vote.
 *
 * `drepActivity` on a directory entry is the epochs *remaining* before the
 * lapse. The elapsed count is deliberately not derived here: it would need the
 * `dRepActivity` protocol parameter, which the wallet API does not serve, and
 * hardcoding it would let a governance action silently falsify a claim about a
 * named DRep.
 */
/**
 * How close to lapsing a DRep has to be before it is called expiring.
 *
 * Six epochs is about a month on mainnet and preprod, whose epochs run five
 * days: long enough that a delegator has time to notice and re-delegate before
 * their voting power stops counting.
 *
 * Deliberately a count of epochs rather than a span of days, even though days
 * are what the badge shows a reader. Epoch length is a property of the network
 * rather than a constant, and the test networks run much shorter epochs than
 * mainnet for their own reasons. An epoch count stays proportionate to the
 * activity window on all of them; a fixed number of days would exceed the whole
 * twenty-epoch window on a short-epoch network, leaving every DRep permanently
 * marked as expiring.
 */
export const INACTIVE_SOON_EPOCHS = 6;

const SECONDS_PER_DAY = 86400;

export function isInactiveSoon(
  drepActivity: number | null | undefined
): boolean {
  return drepActivity != null && drepActivity <= INACTIVE_SOON_EPOCHS;
}

/**
 * Epochs expressed in days, using the chain's own epoch length rather than an
 * assumed five days. Returns null when the network parameters have not loaded,
 * so callers can fall back to stating epochs alone.
 */
export function epochsToDays(
  epochs: number | null | undefined,
  epochLength: number | null | undefined,
  slotLength: number | null | undefined
): number | null {
  if (epochs == null || !epochLength || !slotLength) return null;
  return Math.round((epochs * epochLength * slotLength) / SECONDS_PER_DAY);
}

/**
 * A DRep's standing, as one badge rather than several.
 *
 * The three states are ordered, not independent: a DRep has to be active to be
 * close to going inactive, and one that has already lapsed is inactive rather
 * than both. Two badges side by side let those combinations be rendered, and
 * the pairings that resulted ("Active" beside "Inactive Soon", or worse
 * "Inactive" beside it) either restated one another or contradicted each other
 * outright.
 *
 * Named for what happens rather than for expiry. A DRep does not expire: its
 * voting power stops being counted, and any activity at all, a vote included,
 * starts it counting again.
 */
export type DRepStanding = 'active' | 'inactiveSoon' | 'inactive';

export function getDRepStanding(
  status: 'active' | 'inactive',
  drepActivity: number | null | undefined
): DRepStanding {
  if (status !== 'active') return 'inactive';
  return isInactiveSoon(drepActivity) ? 'inactiveSoon' : 'active';
}
