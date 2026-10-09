import { CardanoNodeStates } from './watchdog.types';

// ── Bug 4 regression: READY state added to CardanoNodeStates ──────────────────
//
// Before the fix, CardanoNodeStates had no READY entry.  DaedalusDiagnostics
// checked FINAL_CARDANO_NODE_STATES (which included RUNNING → 'node-starting')
// to decide whether the "Restart Cardano Node" button is enabled.  When
// loadingPhase === 'ready' the button was always disabled.
//
// After the fix, READY: 'ready' is present and added to FINAL_CARDANO_NODE_STATES.

describe('CardanoNodeStates', () => {
  it('READY maps to the ready loadingPhase', () => {
    expect(CardanoNodeStates.READY).toBe('ready');
  });

  it('RUNNING maps to node-starting loadingPhase', () => {
    expect(CardanoNodeStates.RUNNING).toBe('node-starting');
  });

  it('CRASHED maps to error loadingPhase', () => {
    expect(CardanoNodeStates.CRASHED).toBe('error');
  });

  it('ERRORED maps to error loadingPhase', () => {
    expect(CardanoNodeStates.ERRORED).toBe('error');
  });

  it('UNRECOVERABLE maps to error loadingPhase', () => {
    expect(CardanoNodeStates.UNRECOVERABLE).toBe('error');
  });

  it('maps the starting and updating states to node-starting', () => {
    const nodeStartingStates = [
      CardanoNodeStates.STARTING,
      CardanoNodeStates.RUNNING,
      CardanoNodeStates.UPDATING,
      CardanoNodeStates.UPDATED,
    ];
    for (const state of nodeStartingStates) {
      expect(state).toBe('node-starting');
    }
  });

  it('maps STOPPING and EXITING to the stopping loadingPhase', () => {
    expect(CardanoNodeStates.STOPPING).toBe('stopping');
    expect(CardanoNodeStates.EXITING).toBe('stopping');
  });
});
