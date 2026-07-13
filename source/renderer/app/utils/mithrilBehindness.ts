import type { TipInfo } from '../api/network/types';

/**
 * Availability gate, not a significance threshold: behind-ness is "known" only
 * once both local and network tips have finite epochs. Backend isSignificantlyBehind
 * stays the sole offer signal; this surfaces epochs only, never % or immutable counts.
 */
export const isMithrilBehindnessKnown = (
  localTip: TipInfo | null | undefined,
  networkTip: TipInfo | null | undefined
): boolean =>
  Number.isFinite(localTip?.epoch) && Number.isFinite(networkTip?.epoch);

/**
 * Display-only epochs-behind figure. Returns undefined at or above the anchor,
 * so callers hide "behind" copy near the tip rather than showing a misleading 1.
 * Anchor prefers networkTip.epoch when finite, else the earlier-resolving certifiedEpoch.
 */
export const computeBehindByEpochs = (
  localTip: TipInfo | null | undefined,
  networkTip: TipInfo | null | undefined,
  certifiedEpoch?: number | null | undefined
): number | undefined => {
  const localEpoch = localTip?.epoch;
  if (localEpoch === undefined || !Number.isFinite(localEpoch)) {
    return undefined;
  }
  const networkEpoch = networkTip?.epoch;
  let anchor: number | undefined;
  if (networkEpoch !== undefined && Number.isFinite(networkEpoch)) {
    anchor = networkEpoch;
  } else if (certifiedEpoch != null && Number.isFinite(certifiedEpoch)) {
    anchor = certifiedEpoch;
  }
  if (anchor === undefined) {
    return undefined;
  }
  const diff = anchor - localEpoch;
  return diff > 0 ? diff : undefined;
};
