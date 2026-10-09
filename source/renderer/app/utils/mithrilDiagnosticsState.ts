import { computeBehindByEpochs } from './mithrilBehindness';

type SignificantlyBehind = {
  localImmutableCount: number;
  latestCertifiedImmutable: number;
};

export type MithrilDiagnosticsState = {
  isMithrilPartialSyncWorking: boolean;
  isSignificantlyBehind: boolean;
  behindByEpochs: number | undefined;
};

// Phases after which the watchdog is no longer running a Mithril sync.
const IDLE_MITHRIL_PHASES = ['completed', 'cancelled'];

/**
 * Derives the props the Diagnostics Mithril Sync section needs from the
 * watchdog state mirrored in BackendStore. A sync counts as running while the
 * phase is set and not terminal, matching the 'mithril-syncing' loading phase.
 */
export const getMithrilDiagnosticsState = ({
  mithrilPhase,
  mithrilSignificantlyBehind,
  isStopping,
}: {
  mithrilPhase: string | null;
  mithrilSignificantlyBehind: SignificantlyBehind | null;
  isStopping: boolean;
}): MithrilDiagnosticsState => ({
  isMithrilPartialSyncWorking:
    isStopping ||
    (mithrilPhase !== null && !IDLE_MITHRIL_PHASES.includes(mithrilPhase)),
  isSignificantlyBehind: mithrilSignificantlyBehind !== null,
  behindByEpochs: mithrilSignificantlyBehind
    ? computeBehindByEpochs(
        mithrilSignificantlyBehind.localImmutableCount,
        mithrilSignificantlyBehind.latestCertifiedImmutable
      )
    : undefined,
});
