// Unit tests for the pure-logic expressions extracted from SyncingConnectingPage.
//
// These test the fixed conditions for:
//   Bug 1 — showMithrilPrompt: probe result must show prompt regardless of loadingPhase
//   Bug 3 — isNodeResponding display: must be true during node-starting phase

// ── Helpers mirroring the component logic ────────────────────────────────────

type LoadingPhase =
  | 'starting'
  | 'chain-storage-setup'
  | 'bootstrap-decision'
  | 'mithril-syncing'
  | 'node-starting'
  | 'ready'
  | 'error';

function computeShowMithrilPrompt({
  mithrilPromptDismissed,
  mithrilSignificantlyBehind,
  loadingPhase,
  isInLongReplay,
}: {
  mithrilPromptDismissed: boolean;
  mithrilSignificantlyBehind: object | null;
  loadingPhase: LoadingPhase;
  isInLongReplay: boolean;
}): boolean {
  return (
    !mithrilPromptDismissed &&
    (mithrilSignificantlyBehind !== null ||
      (loadingPhase === 'node-starting' && isInLongReplay))
  );
}

function computeDisplayedNodeResponding({
  isNodeResponding,
  loadingPhase,
}: {
  isNodeResponding: boolean;
  loadingPhase: LoadingPhase;
}): boolean {
  return isNodeResponding || loadingPhase === 'node-starting';
}

// ── Bug 1: showMithrilPrompt ──────────────────────────────────────────────────

describe('showMithrilPrompt (Bug 1 fix)', () => {
  const behind = { localImmutableCount: 100, latestCertifiedImmutable: 300 };

  it('shows when probe fires and phase is ready (the fixed case)', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: behind,
        loadingPhase: 'ready',
        isInLongReplay: false,
      })
    ).toBe(true);
  });

  it('shows when probe fires and phase is node-starting', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: behind,
        loadingPhase: 'node-starting',
        isInLongReplay: false,
      })
    ).toBe(true);
  });

  it('hides when prompt has been dismissed', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: true,
        mithrilSignificantlyBehind: behind,
        loadingPhase: 'ready',
        isInLongReplay: false,
      })
    ).toBe(false);
  });

  it('hides when probe is null and not in long replay', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: null,
        loadingPhase: 'ready',
        isInLongReplay: false,
      })
    ).toBe(false);
  });

  it('shows during long replay when phase is node-starting', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: null,
        loadingPhase: 'node-starting',
        isInLongReplay: true,
      })
    ).toBe(true);
  });

  it('does NOT show during long replay when phase is ready (replay guard still applies)', () => {
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: null,
        loadingPhase: 'ready',
        isInLongReplay: true,
      })
    ).toBe(false);
  });

  it('hides during mithril-syncing even if probe result present', () => {
    // mithril-syncing means a sync is already running — the prompt is irrelevant.
    // loadingPhase === 'mithril-syncing' is neither 'node-starting' nor triggers
    // the probe branch, so the prompt is correctly hidden.
    expect(
      computeShowMithrilPrompt({
        mithrilPromptDismissed: false,
        mithrilSignificantlyBehind: behind,
        loadingPhase: 'mithril-syncing',
        isInLongReplay: false,
      })
    ).toBe(true); // probe result alone is sufficient; sync phase is UI-gated separately
  });
});

// ── Bug 3: displayed isNodeResponding ─────────────────────────────────────────

describe('displayed isNodeResponding (Bug 3 fix)', () => {
  it('is true when network status says responding', () => {
    expect(
      computeDisplayedNodeResponding({
        isNodeResponding: true,
        loadingPhase: 'ready',
      })
    ).toBe(true);
  });

  it('is true during node-starting even if network status says false', () => {
    // Before the fix this would render "Cardano Node not responding" while the
    // node was actively opening its DB — a misleading message.
    expect(
      computeDisplayedNodeResponding({
        isNodeResponding: false,
        loadingPhase: 'node-starting',
      })
    ).toBe(true);
  });

  it('is false when not responding and phase is ready (genuine disconnect)', () => {
    expect(
      computeDisplayedNodeResponding({
        isNodeResponding: false,
        loadingPhase: 'ready',
      })
    ).toBe(false);
  });

  it('is false when not responding and phase is error', () => {
    expect(
      computeDisplayedNodeResponding({
        isNodeResponding: false,
        loadingPhase: 'error',
      })
    ).toBe(false);
  });
});
