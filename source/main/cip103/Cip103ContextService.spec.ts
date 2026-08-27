import cbor from 'cbor';

import { bytesForSpan } from '../../common/cardano/cborSlices';
import type { TransactionContextSnapshot } from '../../common/cardano/transactionContext';
import { preflightCip103Sign } from '../../renderer/app/domains/Cip103Batch';
import {
  DappContextBinding,
  DappTransactionContextServiceError,
} from '../cardano/DappTransactionContextService';
import {
  Cip103ContextError,
  Cip103ContextService,
} from './Cip103ContextService';

const walletId = 'aa'.repeat(20);
const network = {
  networkId: 0 as const,
  networkMagic: 42,
  genesisHash: 'bb'.repeat(32),
};
const binding: DappContextBinding = { walletId, network, generation: 7 };
const inputId = '11'.repeat(32);
const address = Buffer.from(`60${'aa'.repeat(28)}`, 'hex');
const body = new Map<number, unknown>([
  [0, [[Buffer.from(inputId, 'hex'), 0]]],
  [1, [[address, 900_000]]],
  [2, 100_000],
]);
const cborHex = cbor
  .encodeCanonical([body, new Map(), true, null])
  .toString('hex');
const batch = preflightCip103Sign([{ cbor: cborHex, partialSign: true }], 0);
const outputCbor = bytesForSpan(
  batch.items[0].envelope.cbor,
  batch.items[0].transaction.outputs[0].exactSpan
).toString('hex');

const snapshot = (outputs = true): TransactionContextSnapshot => ({
  walletId,
  network,
  chainPoint: { kind: 'genesis' },
  walletGeneration: BigInt(1),
  pendingGeneration: BigInt(1),
  contextDigest: 'cc'.repeat(32),
  contextToken: 'dd'.repeat(32),
  records: [],
  transactions: [cborHex],
  outputs: outputs
    ? [
        {
          outpoint: { transactionId: inputId, index: 0 },
          sourceCbor: outputCbor,
          inputCbor: '',
          canonicalCbor: outputCbor,
          unspentCbor: '',
          provenance: ['node'],
          roles: ['normal'],
          walletMember: true,
          pendingState: 'none',
        },
      ]
    : [],
  ownership: [],
  requiredProofs: [],
  commitmentContexts: [],
  transactionsSemantic: batch.items.map(({ transaction }) => transaction),
  preExistingWitnesses: [],
});

const expectContextFailure = async (
  value: Promise<unknown>,
  failure: Cip103ContextError['failure'],
  transactionIndex?: number
): Promise<void> => {
  try {
    await value;
    throw new Error('Expected context failure');
  } catch (error) {
    expect(error).toBeInstanceOf(Cip103ContextError);
    expect(error).toMatchObject({ failure, transactionIndex });
  }
};

describe('Cip103ContextService', () => {
  it('captures one exact frozen vector and returns one immutable resolution', async () => {
    const captured = snapshot();
    const capture = jest.fn(
      async (_binding, transactions: readonly string[]) => {
        expect(Object.isFrozen(transactions)).toBe(true);
        return captured;
      }
    );
    const service = new Cip103ContextService({ capture });

    const result = await service.capture(binding, batch);

    expect(capture).toHaveBeenCalledTimes(1);
    expect(capture).toHaveBeenCalledWith(binding, [cborHex]);
    expect(result).toMatchObject({
      state: 'context-resolved',
      operation: 'sign',
      resolution: {
        items: [
          {
            transactionIndex: 0,
            bodyHash: batch.items[0].bodyHash,
            inputs: { normal: [{ source: 'node' }] },
          },
        ],
      },
    });
    expect(result.snapshot).toBe(captured);
    expect(Object.isFrozen(result)).toBe(true);
  });

  it('maps binding/backend failures without leaking transport details', async () => {
    const service = new Cip103ContextService({
      capture: jest.fn(async () => {
        throw new DappTransactionContextServiceError('account_changed');
      }),
    });

    await expectContextFailure(
      service.capture(binding, batch),
      'account_changed'
    );
  });

  it('rejects a context that is not bound to the exact ordered vector', async () => {
    const changed = {
      ...snapshot(),
      transactions: ['00'],
    };
    const service = new Cip103ContextService({
      capture: jest.fn(async () => changed),
    });

    await expectContextFailure(
      service.capture(binding, batch),
      'internal_error'
    );
  });

  it('rejects the whole partial-sign batch on indexed resolution failure', async () => {
    const service = new Cip103ContextService({
      capture: jest.fn(async () => snapshot(false)),
    });

    await expectContextFailure(
      service.capture(binding, batch),
      'resolution_failed',
      0
    );
  });
});
