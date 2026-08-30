import cbor from 'cbor';

import { bytesForSpan } from './cborSlices';
import type {
  ContextOutput,
  TransactionContextSnapshot,
} from './transactionContext';
import {
  Cip103OverlayError,
  resolveCip103TransactionOverlay,
} from './transactionOverlay';
import { preflightCip103Sign } from '../../renderer/app/domains/Cip103Batch';
import type { Cip103PreflightItem } from '../types/cip103.types';

const address = Buffer.from(`60${'aa'.repeat(28)}`, 'hex');
const nodeId = '11'.repeat(32);

type Input = string | readonly [string, number];

const encodedInput = (input: Input): [Buffer, number] =>
  typeof input === 'string'
    ? [Buffer.from(input, 'hex'), 0]
    : [Buffer.from(input[0], 'hex'), input[1]];

const transaction = ({
  normal = [nodeId],
  collateral = [],
  reference = [],
  valid = true,
  collateralReturn = false,
}: {
  normal?: Input[];
  collateral?: Input[];
  reference?: Input[];
  valid?: boolean;
  collateralReturn?: boolean;
}): string => {
  const body = new Map<number, unknown>([
    [0, normal.map(encodedInput)],
    [1, [[address, 900_000]]],
    [2, 100_000],
  ]);
  if (collateral.length) body.set(13, collateral.map(encodedInput));
  if (reference.length) body.set(18, reference.map(encodedInput));
  if (collateralReturn) {
    body.set(16, [address, 400_000]);
    body.set(17, 500_000);
  }
  return cbor.encodeCanonical([body, new Map(), valid, null]).toString('hex');
};

const contextOutput = (
  transactionId: string,
  sourceCbor: string,
  provenance: ContextOutput['provenance'],
  roles: ContextOutput['roles'],
  index = 0
): ContextOutput => ({
  outpoint: { transactionId, index },
  sourceCbor,
  inputCbor: '',
  canonicalCbor: sourceCbor,
  unspentCbor: '',
  provenance,
  roles,
  walletMember: true,
  pendingState: 'none',
});

const snapshot = (
  items: readonly Cip103PreflightItem[],
  outputs: readonly ContextOutput[]
): TransactionContextSnapshot => ({
  walletId: 'aa'.repeat(20),
  network: { networkId: 0, networkMagic: 42, genesisHash: 'bb'.repeat(32) },
  chainPoint: { kind: 'genesis' },
  walletGeneration: BigInt(1),
  pendingGeneration: BigInt(1),
  contextDigest: 'cc'.repeat(32),
  contextToken: 'dd'.repeat(32),
  records: [],
  transactions: items.map(({ cbor }) => cbor),
  outputs,
  pendingTransactions: [],
  ownership: [],
  requiredProofs: [],
  commitmentContexts: [],
  transactionsSemantic: items.map(({ transaction: item }) => item),
  preExistingWitnesses: [],
});

const parentOutput = (item: Cip103PreflightItem): string =>
  bytesForSpan(
    item.envelope.cbor,
    item.transaction.outputs[0].exactSpan
  ).toString('hex');

const collateralReturnOutput = (item: Cip103PreflightItem): string => {
  const output = item.transaction.collateral.return;
  if (!output) throw new Error('Missing collateral return');
  return bytesForSpan(item.envelope.cbor, output.exactSpan).toString('hex');
};

const expectOverlayFailure = (
  callback: () => unknown,
  failure: Cip103OverlayError['failure'],
  index: number
): void => {
  try {
    callback();
    throw new Error('Expected overlay failure');
  } catch (error) {
    expect(error).toBeInstanceOf(Cip103OverlayError);
    expect(error).toMatchObject({ failure, transactionIndex: index });
  }
};

describe('CIP-103 transaction overlay', () => {
  it('preserves ordered normal, collateral, and reference parent resolution', () => {
    const parent = preflightCip103Sign([{ cbor: transaction({}) }], 0).items[0];
    const items = preflightCip103Sign(
      [
        { cbor: parent.cbor },
        { cbor: transaction({ normal: [parent.bodyHash] }) },
        {
          cbor: transaction({
            normal: [nodeId],
            collateral: [parent.bodyHash],
          }),
        },
        {
          cbor: transaction({
            normal: [nodeId],
            reference: [parent.bodyHash],
          }),
        },
      ],
      0
    ).items;
    const exactParentOutput = parentOutput(items[0]);
    const result = resolveCip103TransactionOverlay(
      items,
      snapshot(items, [
        contextOutput(nodeId, exactParentOutput, ['node'], ['normal']),
        contextOutput(
          parent.bodyHash,
          exactParentOutput,
          ['earlier', 'pending', 'node'],
          ['normal', 'collateral', 'reference']
        ),
      ])
    );

    expect(
      result.items.map(({ transactionIndex }) => transactionIndex)
    ).toEqual([0, 1, 2, 3]);
    expect(result.items[1].inputs.normal[0]).toMatchObject({
      source: 'earlier',
      sourceTransactionIndex: 0,
      sourceCbor: exactParentOutput,
    });
    expect(result.items[2].inputs.collateral[0].inputRole).toBe('collateral');
    expect(result.items[3].inputs.reference[0].inputRole).toBe('reference');
    expect(result.items[3].inputs.reference[0].provenance).toEqual([
      'earlier',
      'pending',
      'node',
    ]);
    expect(result.items[2].conflicts).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          inputRole: 'collateral',
          earlierTransactionIndex: 1,
        }),
      ])
    );
    expect(
      result.items.some(({ conflicts }) =>
        conflicts.some(({ inputRole }) => inputRole === 'reference')
      )
    ).toBe(false);
    expect(result.items[3].effects.referenced).toHaveLength(1);
    expect(result.items[3].effects.spent).not.toContain(
      result.items[3].inputs.reference[0]
    );
    expect(result.items[0].effects).not.toBe(result.items[1].effects);
    expect(result.items[0].effects).toMatchObject({
      outcome: 'valid',
      produced: [{ kind: 'output', outputIndex: 0 }],
    });
    expect(Object.isFrozen(result)).toBe(true);
    expect(Object.isFrozen(result.items)).toBe(true);
    expect(Object.isFrozen(result.items[3].inputs.reference)).toBe(true);
  });

  it('uses invalid collateral claims and produces only collateral return', () => {
    const collateralId = '33'.repeat(32);
    const parent = preflightCip103Sign(
      [
        {
          cbor: transaction({
            normal: [nodeId],
            collateral: [collateralId],
            valid: false,
            collateralReturn: true,
          }),
        },
      ],
      0
    ).items[0];
    const items = preflightCip103Sign(
      [
        { cbor: parent.cbor },
        {
          cbor: transaction({ normal: [[parent.bodyHash, 1]] }),
        },
        { cbor: transaction({ normal: [collateralId] }) },
      ],
      0
    ).items;
    const normalOutput = parentOutput(parent);
    const returnedOutput = collateralReturnOutput(parent);
    const result = resolveCip103TransactionOverlay(
      items,
      snapshot(items, [
        contextOutput(nodeId, normalOutput, ['node'], ['normal']),
        contextOutput(
          collateralId,
          normalOutput,
          ['node'],
          ['normal', 'collateral']
        ),
        contextOutput(
          parent.bodyHash,
          returnedOutput,
          ['earlier'],
          ['normal'],
          1
        ),
      ])
    );

    expect(result.items[0].effects).toMatchObject({
      outcome: 'invalid',
      spent: [
        {
          inputRole: 'collateral',
          outpoint: { transactionId: collateralId, index: 0 },
        },
      ],
      produced: [
        {
          kind: 'collateral-return',
          outputIndex: 1,
          sourceCbor: returnedOutput,
        },
      ],
    });
    expect(result.items[1].inputs.normal[0]).toMatchObject({
      source: 'earlier',
      sourceTransactionIndex: 0,
    });
    expect(result.items[2].conflicts).toEqual([
      expect.objectContaining({
        inputRole: 'normal',
        earlierTransactionIndex: 0,
      }),
    ]);

    const invalidOutputChild = preflightCip103Sign(
      [
        { cbor: parent.cbor },
        { cbor: transaction({ normal: [[parent.bodyHash, 0]] }) },
      ],
      0
    ).items;
    expectOverlayFailure(
      () =>
        resolveCip103TransactionOverlay(
          invalidOutputChild,
          snapshot(invalidOutputChild, [
            contextOutput(nodeId, normalOutput, ['node'], ['normal']),
            contextOutput(collateralId, normalOutput, ['node'], ['collateral']),
            contextOutput(
              parent.bodyHash,
              normalOutput,
              ['earlier'],
              ['normal']
            ),
          ])
        ),
      'unresolved_input',
      1
    );
  });

  it('rejects self, forward, unresolved, role, and exact-byte conflicts under partial signing', () => {
    const parent = preflightCip103Sign(
      [{ cbor: transaction({}), partialSign: true }],
      0
    ).items[0];
    const forward = preflightCip103Sign(
      [
        {
          cbor: transaction({ normal: [parent.bodyHash] }),
          partialSign: true,
        },
        { cbor: parent.cbor, partialSign: true },
      ],
      0
    ).items;
    expectOverlayFailure(
      () =>
        resolveCip103TransactionOverlay(
          forward,
          snapshot(forward, [
            contextOutput(
              parent.bodyHash,
              parentOutput(forward[1]),
              ['node'],
              ['normal']
            ),
          ])
        ),
      'self_or_forward_input',
      0
    );

    const self = Object.freeze({
      ...parent,
      transaction: Object.freeze({
        ...parent.transaction,
        inputs: Object.freeze({
          ...parent.transaction.inputs,
          normal: Object.freeze([
            Object.freeze({
              ...parent.transaction.inputs.normal[0],
              transactionId: parent.bodyHash,
            }),
          ]),
        }),
      }),
    });
    expectOverlayFailure(
      () => resolveCip103TransactionOverlay([self], snapshot([self], [])),
      'self_or_forward_input',
      0
    );

    expectOverlayFailure(
      () => resolveCip103TransactionOverlay([parent], snapshot([parent], [])),
      'unresolved_input',
      0
    );
    expectOverlayFailure(
      () =>
        resolveCip103TransactionOverlay(
          [parent],
          snapshot(
            [parent],
            [
              contextOutput(
                nodeId,
                parentOutput(parent),
                ['node'],
                ['reference']
              ),
            ]
          )
        ),
      'role_mismatch',
      0
    );

    const childItems = preflightCip103Sign(
      [
        { cbor: parent.cbor, partialSign: true },
        {
          cbor: transaction({ normal: [parent.bodyHash] }),
          partialSign: true,
        },
      ],
      0
    ).items;
    expectOverlayFailure(
      () =>
        resolveCip103TransactionOverlay(
          childItems,
          snapshot(childItems, [
            contextOutput(nodeId, parentOutput(parent), ['node'], ['normal']),
            contextOutput(parent.bodyHash, '80', ['earlier'], ['normal']),
          ])
        ),
      'source_conflict',
      1
    );
  });

  it('preserves equal pending and node authority while selecting pending', () => {
    const pendingId = '44'.repeat(32);
    const items = preflightCip103Sign(
      [{ cbor: transaction({ normal: [pendingId] }) }],
      0
    ).items;
    const sourceCbor = parentOutput(items[0]);
    const result = resolveCip103TransactionOverlay(
      items,
      snapshot(items, [
        contextOutput(pendingId, sourceCbor, ['pending', 'node'], ['normal']),
      ])
    );

    expect(result.items[0].inputs.normal[0]).toMatchObject({
      source: 'pending',
      sourceCbor,
      provenance: ['pending', 'node'],
    });
  });

  it('keeps duplicate items independent and binds children to the latest earlier index', () => {
    const parent = preflightCip103Sign([{ cbor: transaction({}) }], 0).items[0];
    const items = preflightCip103Sign(
      [
        { cbor: parent.cbor },
        { cbor: parent.cbor, partialSign: true },
        { cbor: transaction({ normal: [parent.bodyHash] }) },
      ],
      0
    ).items;
    const sourceCbor = parentOutput(items[0]);
    const result = resolveCip103TransactionOverlay(
      items,
      snapshot(items, [
        contextOutput(nodeId, sourceCbor, ['node'], ['normal']),
        contextOutput(parent.bodyHash, sourceCbor, ['earlier'], ['normal']),
      ])
    );

    expect(result.items).toHaveLength(3);
    expect(result.items[0]).not.toBe(result.items[1]);
    expect(result.items[2].inputs.normal[0].sourceTransactionIndex).toBe(1);
  });
});
