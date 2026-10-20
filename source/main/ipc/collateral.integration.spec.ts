import fs from 'fs';
import os from 'os';
import path from 'path';
import cbor from 'cbor';
import type { CollateralPreparationOperation } from '../../common/types/collateral.types';
import type {
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../common/cip30/executor';
import type { TransactionContextSnapshot } from '../../common/cardano/transactionContext';
import { reconcileTransactionContext } from '../../common/cardano/transactionContext';
import { CollateralService } from '../cip30/CollateralService';
import { CollateralPreferenceStore } from '../cip30/CollateralPreferenceStore';
import { DappRouteLeaseService } from '../dapp/DappRouteLease';
import CollateralStore from '../../renderer/app/stores/CollateralStore';
import { dappCollateralChannel } from '../../renderer/app/ipc/collateral';
import { getCollateralService } from '../cip30/Cip30Broker';
import { getCurrentDappRouteLease } from './dappBrowser';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { onTrustedRendererInvalidated } from './lib/trustedRendererIpcAuthority';
import { handleDappCollateralRequests } from './collateral';
import { DAPP_COLLATERAL_CHANNEL } from '../../common/ipc/api';

jest.mock('../config', () => ({ dappRuntimeConfig: { cip30Enabled: true } }));
jest.mock('../cip30/Cip30Broker', () => ({ getCollateralService: jest.fn() }));
jest.mock('./dappBrowser', () => ({ getCurrentDappRouteLease: jest.fn() }));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onRequest: jest.fn() })),
}));
jest.mock('./lib/trustedRendererIpcAuthority', () => ({
  onTrustedRendererInvalidated: jest.fn(),
}));
jest.mock('../../renderer/app/ipc/collateral', () => ({
  dappCollateralChannel: { request: jest.fn() },
}));
// Context is the backend boundary. Selection, CBOR decoding, persistence,
// operation validation, routing, IPC parsing and renderer state are real.
jest.mock('../../common/cardano/transactionContext', () => ({
  ...(jest.requireActual('../../common/cardano/transactionContext') as object),
  reconcileTransactionContext: jest.fn(),
}));

const walletId = 'ab'.repeat(20);
const otherWalletId = 'ac'.repeat(20);
const genesisHash = 'cd'.repeat(32);
const transactionId = 'ef'.repeat(32);
const network = { networkId: 0 as const, networkMagic: 42, genesisHash };
const sourceRevision = '01'.repeat(20);
const address = Buffer.from(`60${'aa'.repeat(28)}`, 'hex');
const input = { transactionId, index: 1 };
const output = cbor.encodeCanonical([address, 5_000_000]).toString('hex');
const context = (
  visible: boolean,
  pending: boolean
): TransactionContextSnapshot => ({
  walletId,
  network,
  chainPoint: { kind: 'genesis' },
  walletGeneration: BigInt(1),
  pendingGeneration: BigInt(1),
  contextDigest: '11'.repeat(32),
  contextToken: '22'.repeat(32),
  records: [],
  transactions: [],
  outputs: visible
    ? [
        {
          outpoint: input,
          sourceCbor: output,
          inputCbor: cbor
            .encodeCanonical([Buffer.from(transactionId, 'hex'), 1])
            .toString('hex'),
          canonicalCbor: output,
          unspentCbor: cbor
            .encodeCanonical([
              [Buffer.from(transactionId, 'hex'), 1],
              [address, 5_000_000],
            ])
            .toString('hex'),
          provenance: ['node'],
          roles: ['wallet_snapshot'],
          walletMember: true,
          pendingState: pending ? 'outcome_unknown' : 'none',
        },
      ]
    : [],
  pendingTransactions: [],
  ownership: [],
  requiredProofs: [],
  commitmentContexts: [],
  transactionsSemantic: [],
  preExistingWitnesses: [],
  maxCollateralInputs: 3,
});

// Capture the module-owned channel before Jest clears constructor call history.
const channelIndex = (
  MainIpcChannel as unknown as jest.Mock
).mock.calls.findIndex(([name]) => name === DAPP_COLLATERAL_CHANNEL);
const collateralChannel = (MainIpcChannel as unknown as jest.Mock).mock.results[
  channelIndex
].value;

describe('collateral Apps → Send operation integration', () => {
  let directory: string;
  let routes: DappRouteLeaseService;
  let records: CollateralPreferenceStore;
  let service: CollateralService;
  let store: CollateralStore;
  let visible: boolean;
  let pending: boolean;
  let contextCalls: number;
  let handler: (request: unknown) => Promise<unknown>;
  let invalidations: Set<() => void>;
  const navigate = (route: string) =>
    routes.observeTrustedRoute(`http://localhost/#${route}`);
  const operation = (): CollateralPreparationOperation => {
    const value = store.preparation;
    if (!value) throw new Error('Preparation identity missing');
    return value;
  };
  const track = (
    preparation: CollateralPreparationOperation,
    id = transactionId
  ) => handler({ type: 'track-preparation', preparation, transactionId: id });

  beforeEach(() => {
    global.cip30Enabled = true;
    directory = fs.mkdtempSync(path.join(os.tmpdir(), 'collateral-route-'));
    records = new CollateralPreferenceStore(path.join(directory, 'state.json'));
    routes = new DappRouteLeaseService(genesisHash);
    invalidations = new Set();
    visible = false;
    pending = false;
    contextCalls = 0;
    (reconcileTransactionContext as jest.Mock).mockImplementation(() =>
      context(visible && routes.current?.walletId === walletId, pending)
    );
    service = new CollateralService(
      records,
      async (request: Cip30WalletRequest): Promise<Cip30WalletResponse> => {
        if (!routes.current)
          throw new Error('Off-route context must never execute');
        if (request.operation !== 'context')
          throw new Error('Unexpected backend operation');
        contextCalls += 1;
        return { status: 'fulfilled', operation: 'context', value: {} };
      },
      network,
      sourceRevision
    );
    (getCollateralService as jest.Mock).mockReturnValue(service);
    (getCurrentDappRouteLease as jest.Mock).mockImplementation(
      () => routes.current
    );
    (onTrustedRendererInvalidated as jest.Mock).mockImplementation(
      (invalidate: () => void) => {
        invalidations.add(invalidate);
        return () => invalidations.delete(invalidate);
      }
    );
    handleDappCollateralRequests();
    handler =
      collateralChannel.onRequest.mock.calls[
        collateralChannel.onRequest.mock.calls.length - 1
      ][0];
    (dappCollateralChannel.request as jest.Mock).mockImplementation(handler);
    store = new CollateralStore(
      null as never,
      {
        router: {
          goToRoute: {
            trigger: ({ route }: { route: string }) => navigate(route),
          },
        },
      } as never,
      null as never
    );
    store.configure({
      wallets: {
        get activeDappWallet() {
          return routes.current ? { id: routes.current.walletId } : undefined;
        },
        getWalletRoute: (id: string, page: string) => `/wallets/${id}/${page}`,
      },
    } as never);
    navigate(`/apps/${walletId}`);
  });

  afterEach(() => {
    store.teardown();
    fs.rmSync(directory, { recursive: true, force: true });
  });

  it('tracks the 5 ADA self-transfer off Apps, then persists only its confirmed exact outpoint', async () => {
    await store.prepare();
    const preparation = operation();
    expect(routes.current).toBeNull();
    expect(store.preparationFormActive).toBe(true);
    const callsBeforeTracking = contextCalls;
    await store.trackPreparation(walletId, transactionId);
    expect(store.actionFailed).toBe(false);
    expect(store.state).toBe('preparing');
    await expect(track(preparation)).resolves.toMatchObject({ preparation });
    expect(contextCalls).toBe(callsBeforeTracking);
    expect(records.get(walletId, genesisHash)).toBeUndefined();
    visible = true;
    pending = true;
    navigate(`/apps/${walletId}`);
    await store.refresh();
    expect(store.state).toBe('preparing');
    expect(records.get(walletId, genesisHash)).toBeUndefined();
    pending = false;
    await store.refresh();
    expect(store.state).toBe('ready');
    expect(store.snapshot?.preparation).toBeUndefined();
    expect(records.get(walletId, genesisHash)?.preferredInputs).toEqual([
      input,
    ]);
    expect(
      new CollateralPreferenceStore(path.join(directory, 'state.json')).get(
        walletId,
        genesisHash
      )?.preferredInputs
    ).toEqual([input]);
    expect(invalidations.size).toBe(0);
  });

  it('rejects forged, missing and changed identities without off-route context work', async () => {
    await store.prepare();
    const preparation = operation();
    await expect(
      handler({ type: 'track-preparation', transactionId })
    ).rejects.toThrow();
    await expect(
      track({
        ...preparation,
        operationId: '00000000-0000-4000-8000-000000000000',
      })
    ).rejects.toThrow();
    await expect(
      track({ ...preparation, walletId: otherWalletId })
    ).rejects.toThrow();
    await expect(
      track({ ...preparation, networkGenesis: '00'.repeat(32) })
    ).rejects.toThrow();
    await expect(track(preparation, 'not-a-transaction')).rejects.toThrow();
    const accessor = Object.defineProperty({ ...preparation }, 'walletId', {
      enumerable: true,
      get: () => walletId,
    });
    await expect(track(accessor)).rejects.toThrow();
    await expect(
      handler({
        type: 'track-preparation',
        preparation,
        transactionId,
        extra: true,
      })
    ).rejects.toThrow();
    await track(preparation);
    await expect(track(preparation, '12'.repeat(32))).rejects.toThrow();
    await expect(
      store.trackPreparation(otherWalletId, transactionId)
    ).rejects.toThrow();
    expect(store.actionFailed).toBe(true);
    expect(contextCalls).toBe(1);
  });

  it('does not adopt another wallet output and can cancel the original operation from Send', async () => {
    await store.prepare();
    const preparation = operation();
    await store.trackPreparation(walletId, transactionId);
    visible = true;
    navigate(`/apps/${otherWalletId}`);
    await store.refresh();
    expect(records.get(otherWalletId, genesisHash)).toBeUndefined();
    navigate(`/wallets/${walletId}/send`);
    await store.cancelPreparation();
    expect(store.actionFailed).toBe(false);
    expect(store.state).toBe('checking');
    expect(invalidations.size).toBe(0);
    await expect(track(preparation)).rejects.toThrow();
  });

  it('invalidates document-bound authority but not same-document Apps → Send navigation', async () => {
    await store.prepare();
    const old = operation();
    expect(invalidations.size).toBe(1);
    await track(old);
    for (const invalidate of [...invalidations]) invalidate();
    await expect(track(old)).rejects.toThrow();
    navigate(`/apps/${walletId}`);
    await store.prepare();
    const replacement = operation();
    expect(replacement.operationId).not.toBe(old.operationId);
    await expect(track(old)).rejects.toThrow();
    await expect(track(replacement)).resolves.toMatchObject({
      preparation: replacement,
    });
  });

  it.each(['remove', 'prune'] as const)(
    'revokes wallet preparation on %s',
    async (kind) => {
      await store.prepare();
      const preparation = operation();
      if (kind === 'remove') service.removeWallet(walletId);
      else service.pruneWallets([otherWalletId]);
      expect(invalidations.size).toBe(0);
      await expect(track(preparation)).rejects.toThrow();
    }
  );
});
