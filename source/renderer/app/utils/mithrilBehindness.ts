import type { TipInfo } from '../api/network/types';

/**
 * Availability signal (NOT a significance threshold) for whether the renderer
 * can express "how far behind" the node is, in epochs.
 *
 * Behind-ness is "known" only once BOTH the local tip and the network tip
 * ("Last network block") are present with finite epoch numbers — i.e. the node
 * has connected and the network tip has arrived. Before that the epochs figure
 * is `undefined`, so the proactive Mithril prompt and any behind-ness copy must
 * stay suppressed (anti-flash gate; CAT-A → CAT-F coupling).
 *
 * This is deliberately a boolean availability check, never a renderer-computed
 * threshold: the backend `isSignificantlyBehind` remains the sole offer signal
 * (locked safety boundary #4). It surfaces no %/immutable values (D13:
 * behind-ness is epochs-only).
 */
export const isMithrilBehindnessKnown = (
  localTip: TipInfo | null | undefined,
  networkTip: TipInfo | null | undefined
): boolean =>
  Number.isFinite(localTip?.epoch) && Number.isFinite(networkTip?.epoch);
