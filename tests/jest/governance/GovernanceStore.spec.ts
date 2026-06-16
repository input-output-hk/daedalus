import BigNumber from 'bignumber.js';
import GovernanceStore from '../../../source/renderer/app/stores/GovernanceStore';
import { governanceDRepListChannel } from '../../../source/renderer/app/ipc/governanceChannel';
import {
  GovernanceQueryErrorType,
  DRepDirectoryEntry,
} from '../../../source/common/types/governance.types';

// Mock the IPC channel so fetchDRepList never reaches Electron's ipcRenderer.
jest.mock('../../../source/renderer/app/ipc/governanceChannel', () => ({
  governanceDRepListChannel: { request: jest.fn() },
}));

const mockRequest = governanceDRepListChannel.request as jest.Mock;

describe('GovernanceStore', () => {
  beforeEach(() => {
    // Reset between cases so the never-resolving impl from the dedup test
    // does not bleed into other cases.
    mockRequest.mockReset();
  });
  it('rehydrates oversized lovelace strings into exact BigNumber values', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    const oversizedLovelace = '9007199254740993';
    const rawEntries: DRepDirectoryEntry[] = [
      {
        anchor: null,
        drepActivity: 8,
        drepId: 'drep1xj23tk3yqyv7cqv7jn9mkz6xq8c7e5m3s2w1v0p9n8m7l6k5j',
        status: 'active',
        votingPower: oversizedLovelace,
      },
    ];

    const [entry] = (store as any)._rehydrateDReps(rawEntries);

    expect(BigNumber.isBigNumber(entry.votingPower)).toBe(true);
    expect(entry.votingPower.toFixed()).toBe(oversizedLovelace);
    expect(entry.votingPower.isEqualTo(new BigNumber(oversizedLovelace))).toBe(
      true
    );
  });

  it('normalizes plain-object IPC error payloads without collapsing to UNKNOWN', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });
  });

  it('decodes JSON-wrapped IPC Error payloads correctly', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      name: 'Error',
      message: JSON.stringify({
        type: GovernanceQueryErrorType.SocketUnavailable,
        message: 'Cardano node socket path is not available.',
        details: 'CARDANO_NODE_SOCKET_PATH missing',
      }),
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });
  });

  it('normalizes __governanceError marker objects directly', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      __governanceError: true,
      type: GovernanceQueryErrorType.QueryFailed,
      message: 'DRep state query failed.',
      details: 'Missing: --mainnet | --testnet-magic NATURAL',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.QueryFailed,
      message: 'DRep state query failed.',
      details: 'Missing: --mainnet | --testnet-magic NATURAL',
    });
  });

  it('resolves the error type from the queryErrorType fallback property', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      queryErrorType: GovernanceQueryErrorType.Timeout,
      message: 'DRep state query timed out.',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.Timeout,
      message: 'DRep state query timed out.',
      details: undefined,
    });
  });

  it('normalizes a generic Error instance via the Error-instance fallback path', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    // A non-governance throw whose .message is neither JSON nor a marked
    // object, and which carries no type/queryErrorType, drops out of the
    // plain-object branches and lands on the `err instanceof Error` path.
    const normalized = (store as any)._normalizeError(new Error('boom'));

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.Unknown,
      message: 'boom',
      details: undefined,
    });
  });

  it('does not fetch the DRep list from setup()', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    store.setup();

    // The query must fire only on Governance-route entry / explicit refresh,
    // never at store init (which runs before the node is synced).
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('deduplicates concurrent route-entry / refresh fetches', () => {
    // Never resolves: holds the store in its in-flight (Loading) state so the
    // second call hits the dedup guard.
    mockRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);

    // Route entry (DRepDirectoryPage.componentDidMount) triggers the fetch.
    void store.refresh();
    // A second trigger (rapid re-entry or refresh) while Loading returns early.
    void store.refresh();

    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(store.isLoading).toBe(true);
  });
});
