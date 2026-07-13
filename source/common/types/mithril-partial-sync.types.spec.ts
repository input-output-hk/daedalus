import {
  isMithrilPartialSyncOverlayStatus,
  makeIdlePartialSyncStatus,
} from './mithril-partial-sync.types';

describe('mithril-partial-sync types helpers', () => {
  it('makeIdlePartialSyncStatus returns the idle snapshot with fresh nested references per call', () => {
    const first = makeIdlePartialSyncStatus();
    const second = makeIdlePartialSyncStatus();

    expect(first).toEqual({
      status: 'idle',
      allowedRecoveryActions: [],
      transferProgress: {},
      progressItems: [],
      error: null,
    });
    expect(second).not.toBe(first);
    expect(second.transferProgress).not.toBe(first.transferProgress);
    expect(second.progressItems).not.toBe(first.progressItems);
    expect(second.allowedRecoveryActions).not.toBe(
      first.allowedRecoveryActions
    );
  });

  it('treats every working and terminal status as an overlay status, but never idle', () => {
    const overlayStatuses = [
      'stopping-node',
      'cancelling',
      'preparing',
      'downloading',
      'verifying',
      'converting',
      'installing',
      'finalizing',
      'starting-node',
      'completed',
      'failed',
      'cancelled',
    ] as const;

    overlayStatuses.forEach((status) => {
      expect(isMithrilPartialSyncOverlayStatus(status)).toBe(true);
    });
    expect(isMithrilPartialSyncOverlayStatus('idle')).toBe(false);
  });
});
