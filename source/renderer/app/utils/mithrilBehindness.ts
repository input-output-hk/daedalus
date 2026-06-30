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

/**
 * DISPLAY-ONLY "how far behind" figure, in epochs, for the proactive Mithril
 * prompt and the Diagnostics Mithril section.
 *
 * Returns `undefined` when the epoch gap is `<= 0` (the node is level with or
 * ahead of the anchor) instead of the old `Math.max(1, …)` clamp that
 * misleadingly rendered "about 1 epochs behind" near the tip (finding #7). Every
 * consumer already has an unknown/fallback branch for `undefined`
 * (`SyncingConnectingMithrilPrompt` `promptBodyUnknown`,
 * `MithrilPartialSyncConfirmation` `behindUnknown`).
 *
 * This is NOT the gate — the gate is the unchanged, clamp-free
 * `isMithrilBehindnessKnown`. The `<= 0 ⇒ undefined` rule lives ONLY here.
 *
 * #16 hybrid anchor (D-702b-10): prefer `networkTip.epoch` when finite (accurate
 * to the live tip near the end of sync, where the certified-beacon lag is most
 * visible), else fall back to `certifiedEpoch` — the Mithril certified-beacon
 * epoch, which is horizon-free and available from the first moment so the figure
 * can show during early/mid sync. When `certifiedEpoch` is omitted/undefined the
 * helper behaves EXACTLY as the networkTip-only version (no regression).
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
