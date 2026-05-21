import { shouldCloseDiagnosticsForPartialSyncOverlay } from './DaedalusDiagnosticsDialog';

describe('shouldCloseDiagnosticsForPartialSyncOverlay', () => {
  it('closes only when partial sync reaches an overlay-backed status', () => {
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('stopping-node', 'preparing')
    ).toBe(true);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('preparing', 'downloading')
    ).toBe(false);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('idle', 'failed')
    ).toBe(true);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('stopping-node', 'idle')
    ).toBe(false);
  });
});
