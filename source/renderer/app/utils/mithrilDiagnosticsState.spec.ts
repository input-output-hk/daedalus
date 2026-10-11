import { getMithrilDiagnosticsState } from './mithrilDiagnosticsState';

const idle = {
  mithrilPhase: null,
  mithrilSignificantlyBehind: null,
  isStopping: false,
};

describe('getMithrilDiagnosticsState', () => {
  it('reports no sync and no lag when the watchdog is idle', () => {
    expect(getMithrilDiagnosticsState(idle)).toEqual({
      isMithrilPartialSyncWorking: false,
      isSignificantlyBehind: false,
      behindByEpochs: undefined,
    });
  });

  it('reports a running sync for every non-terminal phase', () => {
    ['preparing', 'downloading', 'verifying', 'unpacking'].forEach((phase) => {
      expect(
        getMithrilDiagnosticsState({ ...idle, mithrilPhase: phase })
          .isMithrilPartialSyncWorking
      ).toBe(true);
    });
  });

  it('treats completed and cancelled as idle', () => {
    ['completed', 'cancelled'].forEach((phase) => {
      expect(
        getMithrilDiagnosticsState({ ...idle, mithrilPhase: phase })
          .isMithrilPartialSyncWorking
      ).toBe(false);
    });
  });

  it('blocks the action while the backend is stopping', () => {
    expect(
      getMithrilDiagnosticsState({ ...idle, isStopping: true })
        .isMithrilPartialSyncWorking
    ).toBe(true);
  });

  it('converts the immutable chunk gap into whole epochs', () => {
    const state = getMithrilDiagnosticsState({
      ...idle,
      mithrilSignificantlyBehind: {
        localImmutableCount: 1000,
        latestCertifiedImmutable: 1000 + 2160 * 3 + 5,
      },
    });
    expect(state.isSignificantlyBehind).toBe(true);
    expect(state.behindByEpochs).toBe(3);
  });
});
