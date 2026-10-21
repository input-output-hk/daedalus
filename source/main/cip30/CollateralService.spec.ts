import fs from 'fs';
import os from 'os';
import path from 'path';
import * as serialization from '../../common/cardano/cip30Serialization';
import * as selection from '../../common/cardano/collateralSelection';
import * as transactionContext from '../../common/cardano/transactionContext';
import type {
  CollateralPreparationOperation,
  CollateralSnapshot,
} from '../../common/types/collateral.types';
import { CollateralPreferenceStore } from './CollateralPreferenceStore';
import { CollateralService } from './CollateralService';

jest.mock('../../common/cardano/cip30Serialization', () => ({
  ...(jest.requireActual('../../common/cardano/cip30Serialization') as object),
  controlledCip30Utxos: jest.fn(),
}));
jest.mock('../../common/cardano/collateralSelection', () => ({
  ...(jest.requireActual('../../common/cardano/collateralSelection') as object),
  isCip30CollateralCandidate: jest.fn(() => true),
  selectCip30Collateral: jest.fn(),
}));
jest.mock('../../common/cardano/transactionContext', () => ({
  ...(jest.requireActual('../../common/cardano/transactionContext') as object),
  reconcileTransactionContext: jest.fn(),
}));

const walletId = 'ab'.repeat(20);
const genesisHash = 'cd'.repeat(32);
const network = { networkId: 0 as const, networkMagic: 42, genesisHash };
const lease = { walletId, networkGenesis: genesisHash, routeEpoch: 1 };
const input = { transactionId: 'ef'.repeat(32), index: 0 };
const context = (pendingTransactions: readonly unknown[] = []) => ({
  outputs: [],
  pendingTransactions,
  maxCollateralInputs: 3,
});
const utxo = {
  context: { outpoint: input, unspentCbor: 'utxo', pendingState: 'none' },
  value: { coin: BigInt(5_000_000), assets: [] },
};

const operation = (
  snapshot: CollateralSnapshot
): CollateralPreparationOperation => {
  if (!snapshot.preparation) throw new Error('Expected preparation operation');
  return snapshot.preparation;
};

describe('CollateralService', () => {
  let directory: string;
  let records: CollateralPreferenceStore;
  let executeWallet: jest.Mock;
  let service: CollateralService;

  beforeEach(() => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), 'collateral-service-'));
    records = new CollateralPreferenceStore(path.join(directory, 'state.json'));
    executeWallet = jest.fn().mockResolvedValue({
      status: 'fulfilled',
      operation: 'context',
      value: {},
    });
    service = new CollateralService(
      records,
      executeWallet,
      network,
      '01'.repeat(20)
    );
    (
      transactionContext.reconcileTransactionContext as jest.Mock
    ).mockReturnValue(context());
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([]);
    (selection.selectCip30Collateral as jest.Mock).mockReturnValue(null);
  });

  afterEach(() => fs.rmSync(directory, { recursive: true, force: true }));

  it('projects checking work into not-ready and explicit preparing states', async () => {
    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'not-ready', targetLovelace: '5000000' },
    });
    await expect(service.prepare(lease)).resolves.toMatchObject({
      preference: { state: 'preparing' },
    });
  });

  it('adopts an observed existing candidate and persists only its preference', async () => {
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);
    (selection.selectCip30Collateral as jest.Mock).mockReturnValue(['utxo']);

    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'ready', preferredInputs: [input] },
    });
    expect(records.get(walletId, genesisHash)).toMatchObject({
      targetLovelace: '5000000',
      preferredInputs: [input],
    });
  });

  it('records preparation only after its exact output is confirmed', async () => {
    const unregister = jest.fn();
    const preparation = operation(
      await service.prepare(lease, () => unregister)
    );
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([
      {
        ...utxo,
        context: { ...utxo.context, pendingState: 'outcome_unknown' },
      },
    ]);
    (selection.selectCip30Collateral as jest.Mock).mockImplementation(
      (utxos) => (utxos.length ? ['utxo'] : null)
    );

    expect(
      service.trackPreparation(preparation, input.transactionId)
    ).toMatchObject({
      preference: { state: 'preparing', preferredInputs: [] },
      preparation,
    });
    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'preparing', preferredInputs: [] },
    });
    expect(records.get(walletId, genesisHash)).toBeUndefined();

    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);
    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'ready', preferredInputs: [input] },
    });
    expect(unregister).toHaveBeenCalledTimes(1);
    expect(() =>
      service.trackPreparation(preparation, input.transactionId)
    ).toThrow('Collateral preparation is not active');
  });

  it('reuses the initialized operation and retains its submitted ID', async () => {
    const unregister = jest.fn();
    const register = jest.fn(() => unregister);
    const first = operation(await service.prepare(lease, register));
    service.trackPreparation(first, input.transactionId);
    const calls = executeWallet.mock.calls.length;
    const repeated = await service.prepare(
      { ...lease, routeEpoch: 2 },
      register
    );
    expect(repeated.preparation).toBe(first);
    expect(executeWallet).toHaveBeenCalledTimes(calls);
    expect(register).toHaveBeenCalledTimes(1);
    expect(() => service.trackPreparation(first, '11'.repeat(32))).toThrow(
      'Invalid collateral preparation transaction'
    );
    expect(
      service.trackPreparation(first, input.transactionId).preparation
    ).toBe(first);
    expect(executeWallet).toHaveBeenCalledTimes(calls);
  });

  it('removes failed initialization and permits a fresh operation', async () => {
    const unregister = jest.fn();
    executeWallet.mockRejectedValueOnce(new Error('context failed'));
    await expect(service.prepare(lease, () => unregister)).rejects.toThrow(
      'context failed'
    );
    expect(unregister).toHaveBeenCalledTimes(1);
    expect(operation(await service.prepare(lease)).walletId).toBe(walletId);
  });

  it('shares an initialization in progress without exposing its token early', async () => {
    let resolve: (value: unknown) => void = () => {};
    executeWallet.mockReturnValueOnce(
      new Promise((done) => {
        resolve = done;
      })
    );
    const register = jest.fn(() => jest.fn());
    const first = service.prepare(lease, register);
    const repeated = service.prepare({ ...lease, routeEpoch: 2 }, register);
    expect(executeWallet).toHaveBeenCalledTimes(1);
    resolve({ status: 'fulfilled', operation: 'context', value: {} });
    const snapshots = await Promise.all([first, repeated]);
    expect(snapshots[0].preparation).toBe(snapshots[1].preparation);
    expect(register).toHaveBeenCalledTimes(1);
  });

  it('rejects forged, wrong-wallet, wrong-network and stale operations', async () => {
    const currentService = service;
    const preparation = operation(await service.prepare(lease));
    for (const forged of [
      { ...preparation, operationId: '12345678-1234-4123-8123-123456789abc' },
      { ...preparation, walletId: '01'.repeat(20) },
      { ...preparation, networkGenesis: '02'.repeat(32) },
    ]) {
      expect(() =>
        currentService.trackPreparation(forged, input.transactionId)
      ).toThrow('Collateral preparation is not active');
      expect(() => currentService.cancelPreparation(forged)).toThrow(
        'Collateral preparation is not active'
      );
    }
    service.cancelPreparation(preparation);
    const next = operation(await service.prepare(lease));
    expect(next.operationId).not.toBe(preparation.operationId);
    expect(() =>
      service.trackPreparation(preparation, input.transactionId)
    ).toThrow('Collateral preparation is not active');
  });

  it('cancels off-route without context or clearing an existing preference', async () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });
    const unregister = jest.fn();
    const preparation = operation(
      await service.prepare(lease, () => unregister)
    );
    service.trackPreparation(preparation, input.transactionId);
    executeWallet.mockClear();
    expect(service.cancelPreparation(preparation)).toMatchObject({
      preference: { state: 'checking', preferredInputs: [input] },
    });
    expect(executeWallet).not.toHaveBeenCalled();
    expect(records.get(walletId, genesisHash)?.preferredInputs).toEqual([
      input,
    ]);
    expect(unregister).toHaveBeenCalledTimes(1);
  });

  it.each(['clear', 'repair', 'remove', 'prune', 'document'] as const)(
    'invalidates the operation and unregisters on %s',
    async (kind) => {
      let invalidate = () => {};
      const unregister = jest.fn();
      const preparation = operation(
        await service.prepare(lease, (listener) => {
          invalidate = listener;
          return unregister;
        })
      );
      if (kind === 'clear') service.clear(lease);
      if (kind === 'repair') service.repair(lease);
      if (kind === 'remove') service.removeWallet(walletId);
      if (kind === 'prune') service.pruneWallets([]);
      if (kind === 'document') invalidate();
      expect(unregister).toHaveBeenCalledTimes(1);
      expect(() =>
        service.trackPreparation(preparation, input.transactionId)
      ).toThrow('Collateral preparation is not active');
      expect(operation(await service.prepare(lease)).operationId).not.toBe(
        preparation.operationId
      );
    }
  );

  it('does not invalidate another wallet on removal or retained-wallet pruning', async () => {
    const otherLease = { ...lease, walletId: '03'.repeat(20) };
    const retained = operation(await service.prepare(lease));
    const removed = operation(await service.prepare(otherLease));
    service.removeWallet(otherLease.walletId);
    service.pruneWallets([walletId]);
    expect(
      service.trackPreparation(retained, input.transactionId).preparation
    ).toBe(retained);
    expect(() => service.cancelPreparation(removed)).toThrow(
      'Collateral preparation is not active'
    );
  });

  it('handles synchronous invalidation during registration without backend access', async () => {
    const unregister = jest.fn();
    await expect(
      service.prepare(lease, (invalidate) => {
        invalidate();
        return unregister;
      })
    ).rejects.toThrow('Collateral preparation is not active');
    expect(unregister).toHaveBeenCalledTimes(1);
    expect(executeWallet).not.toHaveBeenCalled();
    expect(operation(await service.prepare(lease))).toBeDefined();
  });

  it('does not adopt outputs when the document is lost during initialization', async () => {
    let invalidate = () => {};
    let resolve: (value: unknown) => void = () => {};
    executeWallet.mockReturnValueOnce(
      new Promise((done) => {
        resolve = done;
      })
    );
    const pending = service.prepare(lease, (listener) => {
      invalidate = listener;
      return jest.fn();
    });
    invalidate();
    const replacement = operation(await service.prepare(lease));
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);
    (selection.selectCip30Collateral as jest.Mock).mockReturnValue(['utxo']);
    resolve({ status: 'fulfilled', operation: 'context', value: {} });
    await expect(pending).rejects.toThrow(
      'Collateral preparation is not active'
    );
    expect(records.get(walletId, genesisHash)).toBeUndefined();
    expect(
      service.trackPreparation(replacement, input.transactionId).preparation
    ).toBe(replacement);
  });

  it('does not adopt a confirmed output from another transaction or wallet', async () => {
    const preparation = operation(await service.prepare(lease));
    service.trackPreparation(preparation, '11'.repeat(32));
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);
    (selection.selectCip30Collateral as jest.Mock).mockImplementation(
      (utxos) => (utxos.length ? ['utxo'] : null)
    );
    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'preparing', preferredInputs: [] },
      preparation,
    });
    expect(records.get(walletId, genesisHash)).toBeUndefined();
    const other = operation(
      await service.prepare({ ...lease, walletId: '03'.repeat(20) })
    );
    expect(() =>
      service.trackPreparation(
        { ...preparation, walletId: other.walletId },
        input.transactionId
      )
    ).toThrow('Collateral preparation is not active');
    expect(records.get(other.walletId, genesisHash)).toBeUndefined();
  });

  it('derives charged state from accepted invalid collateral history', async () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });
    executeWallet.mockImplementation(({ operation }) =>
      Promise.resolve(
        operation === 'context'
          ? { status: 'fulfilled', operation, value: {} }
          : {
              status: 'fulfilled',
              operation,
              value: {
                transactions: [
                  {
                    transactionId: '12'.repeat(32),
                    status: 'in_ledger',
                    scriptValidity: 'invalid',
                    normalInputs: [],
                    collateralInputs: [input],
                  },
                ],
              },
            }
      )
    );

    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'charged' },
    });
  });

  it('restores ready after rollback makes the preferred output visible', async () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);

    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'ready' },
    });
  });

  it('detects ordinary preferred-input spending without changing selection', () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });

    expect(service.spendsPreference(lease, [input])).toBe(true);
    expect(
      service.spendsPreference(lease, [
        { transactionId: '01'.repeat(32), index: 0 },
      ])
    ).toBe(false);
  });

  it('scopes native selection preferences to the wallet and current network', () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });
    const otherNetwork = new CollateralService(
      records,
      executeWallet,
      { ...network, genesisHash: '02'.repeat(32) },
      '01'.repeat(20)
    );
    expect(service.preferredInputs(walletId)).toEqual([input]);
    expect(service.preferredInputs('03'.repeat(20))).toEqual([]);
    expect(otherNetwork.preferredInputs(walletId)).toEqual([]);
  });

  it.each([
    ['normalInputs', 'will-be-spent'],
    ['collateralInputs', 'in-use'],
  ] as const)('derives %s pending use as %s', async (field, state) => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });
    (
      transactionContext.reconcileTransactionContext as jest.Mock
    ).mockReturnValue(
      context([{ normalInputs: [], collateralInputs: [], [field]: [input] }])
    );

    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state },
    });
  });

  it('clears preference metadata without querying, signing, or submitting', () => {
    records.put({
      walletId,
      networkGenesis: genesisHash,
      targetLovelace: '5000000',
      preferredInputs: [input],
      generation: 1,
    });

    expect(service.clear(lease)).toMatchObject({
      preference: { state: 'not-ready', preferredInputs: [] },
    });
    expect(records.get(walletId, genesisHash)).toBeUndefined();
    expect(executeWallet).not.toHaveBeenCalled();
  });
});
