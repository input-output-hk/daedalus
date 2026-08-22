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
export const LAPSING_SOON_EPOCHS = 6;

const SECONDS_PER_DAY = 86400;

export function isLapsingSoon(
  drepActivity: number | null | undefined
): boolean {
  return drepActivity != null && drepActivity <= LAPSING_SOON_EPOCHS;
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
