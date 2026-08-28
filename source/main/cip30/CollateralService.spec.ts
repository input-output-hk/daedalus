import fs from 'fs';
import os from 'os';
import path from 'path';
import * as serialization from '../../common/cardano/cip30Serialization';
import * as selection from '../../common/cardano/collateralSelection';
import * as transactionContext from '../../common/cardano/transactionContext';
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
    (transactionContext.reconcileTransactionContext as jest.Mock).mockReturnValue(
      context()
    );
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
    await service.prepare(lease);
    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([
      {
        ...utxo,
        context: { ...utxo.context, pendingState: 'outcome_unknown' },
      },
    ]);
    (selection.selectCip30Collateral as jest.Mock).mockImplementation((utxos) =>
      utxos.length ? ['utxo'] : null
    );

    await expect(
      service.trackPreparation(lease, input.transactionId)
    ).resolves.toMatchObject({
      preference: { state: 'preparing', preferredInputs: [] },
    });
    expect(records.get(walletId, genesisHash)).toBeUndefined();

    (serialization.controlledCip30Utxos as jest.Mock).mockReturnValue([utxo]);
    await expect(service.snapshot(lease)).resolves.toMatchObject({
      preference: { state: 'ready', preferredInputs: [input] },
    });
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
    (transactionContext.reconcileTransactionContext as jest.Mock).mockReturnValue(
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
