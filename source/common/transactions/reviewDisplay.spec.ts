/** @jest-environment node */

import cbor from 'cbor';

import semanticFixture from '../cardano/fixtures/exact-cbor/semantic-conway-v1.json';
import {
  decodeConwayTransaction,
  Output,
  SemanticTransaction,
} from '../cardano/transaction';
import { parseConwayTransactionEnvelope } from '../cardano/transactionEnvelope';
import {
  createTransactionReviewDisplay,
  parseTransactionReviewDisplay,
  TransactionReviewContext,
} from './reviewDisplay';

const decoded = decodeConwayTransaction(
  parseConwayTransactionEnvelope(Buffer.from(semanticFixture.cborHex, 'hex'))
);
const ownedCredential = 'aa'.repeat(28);
const otherCredential = 'bb'.repeat(28);
const ownedAddress = `60${ownedCredential}`;
const otherAddress = `60${otherCredential}`;
const transactionId = '11'.repeat(32);
const collateralId = '22'.repeat(32);
const span = decoded.envelope.spans.body;
const output = (address: string, coin: bigint): Output => ({
  address,
  value: { coin, assets: [] },
  exactSpan: span,
});
const sourceCbor = (address: string, coin: bigint) =>
  cbor
    .encodeCanonical([Buffer.from(address, 'hex'), Number(coin)])
    .toString('hex');
const contextOutput = (
  id: string,
  address: string,
  coin: bigint,
  walletMember: boolean
) => ({
  outpoint: { transactionId: id, index: 0 },
  sourceCbor: sourceCbor(address, coin),
  inputCbor: '',
  canonicalCbor: '',
  unspentCbor: '',
  provenance: ['node' as const],
  roles: ['normal' as const],
  walletMember,
  pendingState: 'none' as const,
});
const context = (
  outputs = [
    contextOutput(transactionId, ownedAddress, BigInt(10_000_000), true),
    contextOutput(collateralId, ownedAddress, BigInt(5_000_000), true),
  ],
  ownership: TransactionReviewContext['ownership'] = [
    {
      credentialKind: 'payment',
      credential: ownedCredential,
      ownership: 'owned_key',
      derivationPath: [0, 0],
      proofKinds: [],
    },
    {
      credentialKind: 'payment',
      credential: otherCredential,
      ownership: 'unowned',
      derivationPath: [],
      proofKinds: [],
    },
  ]
): TransactionReviewContext => ({
  outputs,
  ownership,
  network: { networkId: 0, networkMagic: 42, genesisHash: '00'.repeat(32) },
});
const transaction = (): SemanticTransaction => ({
  ...decoded,
  inputs: {
    normal: [{ transactionId, index: BigInt(0), span }],
    collateral: [{ transactionId: collateralId, index: BigInt(0), span }],
    reference: [],
  },
  outputs: [
    output(ownedAddress, BigInt(3_000_000)),
    output(otherAddress, BigInt(6_000_000)),
  ],
  fee: BigInt(1_000_000),
  collateral: {
    return: output(ownedAddress, BigInt(4_500_000)),
    maximumLoss: { coin: BigInt(500_000), assets: [] },
  },
  effects: [
    { kind: 'input', value: {} },
    { kind: 'output', value: {} },
    { kind: 'output', value: {} },
    { kind: 'collateral-input', value: {} },
    { kind: 'collateral-return', value: {} },
  ],
});

describe('transaction review display', () => {
  it('derives exact normal and script-invalid wallet changes from authenticated ownership', () => {
    const normal = createTransactionReviewDisplay(
      transaction(),
      context(),
      'sign'
    );
    expect(normal.walletInputs).toEqual({ coin: '10000000', assets: [] });
    expect(normal.walletOutputs).toEqual({ coin: '3000000', assets: [] });
    expect(normal.walletChange).toEqual({ coin: '-7000000', assets: [] });
    expect(
      normal.entries.map(({ role, ownership }) => [role, ownership])
    ).toEqual([
      ['input', 'wallet'],
      ['output', 'wallet'],
      ['output', 'other'],
      ['collateral-input', 'wallet'],
      ['collateral-return', 'wallet'],
    ]);

    const invalidSubmission = createTransactionReviewDisplay(
      transaction(),
      context(),
      'submit'
    );
    expect(invalidSubmission.walletInputs?.coin).toBe('5000000');
    expect(invalidSubmission.walletOutputs?.coin).toBe('4500000');
    expect(invalidSubmission.walletChange?.coin).toBe('-500000');
  });

  it('makes every aggregate unavailable when relevant ownership is unknown', () => {
    const unknown = createTransactionReviewDisplay(
      transaction(),
      context(undefined, context().ownership.slice(0, 1)),
      'sign'
    );
    expect(unknown.walletInputs).toBeNull();
    expect(unknown.walletOutputs).toBeNull();
    expect(unknown.walletChange).toBeNull();
  });

  it('rejects contradictory authenticated membership and ownership', () => {
    expect(() =>
      createTransactionReviewDisplay(
        transaction(),
        context(undefined, [
          {
            credentialKind: 'payment',
            credential: ownedCredential,
            ownership: 'unowned',
            derivationPath: [],
            proofKinds: [],
          },
          context().ownership[1],
        ]),
        'sign'
      )
    ).toThrow('contradicts');
  });

  it('strictly validates canonical amounts, identities, positions, and object shape', () => {
    const valid = createTransactionReviewDisplay(
      transaction(),
      context(),
      'sign'
    );
    expect(parseTransactionReviewDisplay(valid)).toEqual(valid);
    expect(() =>
      parseTransactionReviewDisplay({ ...valid, fee: '-0' })
    ).toThrow();
    expect(() =>
      parseTransactionReviewDisplay({ ...valid, extra: true })
    ).toThrow();
    expect(() =>
      parseTransactionReviewDisplay({
        ...valid,
        entries: valid.entries.map((entry, index) =>
          index === 1
            ? { ...entry, effectIndex: valid.entries[0].effectIndex }
            : entry
        ),
      })
    ).toThrow('Duplicate effect index');
    expect(() =>
      parseTransactionReviewDisplay({
        ...valid,
        entries: valid.entries.map((entry, index) =>
          index === 1 ? { ...entry, position: 4 } : entry
        ),
      })
    ).toThrow('Invalid entry positions');
  });
});
