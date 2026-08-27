import { generateKeyPairSync, sign } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import type {
  ContextOutput,
  TransactionContextSnapshot,
} from '../../../common/cardano/transactionContext';
import type { HardwareTransactionCapability } from '../../../common/types/hardware-wallets.types';
import { WitnessSetError } from '../../../common/cardano/witnessSet';
import { preflightCip103Sign } from '../domains/Cip103Batch';
import { prepareHardwareTransaction } from './hardwareWalletTransaction';
import { verifyHardwareTransactionWitnesses } from './hardwareWalletWitnessSet';

const keys = generateKeyPairSync('ed25519');
const publicKey = (keys.publicKey.export({
  format: 'der',
  type: 'spki',
}) as Buffer).subarray(-32);
const credential = Buffer.from(blake2b(publicKey, undefined, 28)).toString(
  'hex'
);
const address = `60${credential}`;
const normalId = '11'.repeat(32);
const referenceId = '22'.repeat(32);
const outputCbor = cbor
  .encodeCanonical([Buffer.from(address, 'hex'), 1_000_000])
  .toString('hex');
const body = new Map<number, unknown>([
  [0, [[Buffer.from(normalId, 'hex'), 0]]],
  [1, [[Buffer.from(address, 'hex'), 900_000]]],
  [2, 100_000],
  [18, [[Buffer.from(referenceId, 'hex'), 0]]],
]);
const transactionCbor = cbor
  .encodeCanonical([body, new Map(), true, null])
  .toString('hex');
const item = preflightCip103Sign([{ cbor: transactionCbor }], 0).items[0];
const contextOutput = (
  transactionId: string,
  role: 'normal' | 'reference'
): ContextOutput => ({
  outpoint: { transactionId, index: 0 },
  sourceCbor: outputCbor,
  inputCbor: '',
  canonicalCbor: outputCbor,
  unspentCbor: '',
  provenance: ['node'],
  roles: [role],
  walletMember: true,
  pendingState: 'none',
});
const snapshot = (owned = true): TransactionContextSnapshot => ({
  walletId: 'aa'.repeat(20),
  network: { networkId: 0, networkMagic: 42, genesisHash: 'bb'.repeat(32) },
  chainPoint: { kind: 'genesis' },
  walletGeneration: BigInt(1),
  pendingGeneration: BigInt(1),
  contextDigest: 'cc'.repeat(32),
  contextToken: 'dd'.repeat(32),
  records: [],
  transactions: [transactionCbor],
  outputs: [
    contextOutput(normalId, 'normal'),
    contextOutput(referenceId, 'reference'),
  ],
  ownership: owned
    ? [
        {
          credentialKind: 'payment',
          credential,
          ownership: 'owned_key',
          derivationPath: [0x8000073c, 0x80000717, 0x80000000, 0, 0],
          proofKinds: ['normal_input'],
        },
      ]
    : [],
  requiredProofs: [
    {
      transactionIndex: 0,
      proofKind: 'normal_input',
      credentialKind: 'payment',
      credential,
      required: true,
    },
  ],
  commitmentContexts: [],
  transactionsSemantic: [item.transaction],
  preExistingWitnesses: [],
});
const rejectedCapability: HardwareTransactionCapability = {
  matrixRevision: 'task-006-matrix-2026-08-14',
  artifactId: 'ledger-8.0.0-candidate',
  rowId: 'ledger-signTx',
  vendor: 'ledger',
  staticallyRepresentable: false,
  staticGatesPassed: false,
  physicalCertified: false,
  productEnabled: false,
  familyDispositions: {
    'root-envelope': 'reject_pre_device',
    'map-order': 'reject_pre_device',
  },
};

const readyCapability: HardwareTransactionCapability = {
  ...rejectedCapability,
  artifactId: 'test-only-ready',
  staticallyRepresentable: true,
  staticGatesPassed: true,
  familyDispositions: { 'root-envelope': 'representable' },
};

describe('hardware transaction preparation', () => {
  it('derives exact trusted paths and rejects current matrix rows pre-device', () => {
    const result = prepareHardwareTransaction(
      snapshot(),
      0,
      false,
      rejectedCapability
    );
    expect(result.status).toBe('rejected');
    expect(result.deviceInteraction).toBe(false);
    expect(result.exact.transaction).toBe(item.transaction);
    expect(result.exact.bodyHash).toBe(item.bodyHash);
    expect(result.exact.signers).toEqual([
      expect.objectContaining({
        keyHash: credential,
        proofKinds: ['normal_input'],
        path: [0x8000073c, 0x80000717, 0x80000000, 0, 0],
      }),
    ]);
    expect(result.exact.ownedOutputs).toEqual([
      expect.objectContaining({ address, paymentPath: expect.any(Array) }),
    ]);
    expect(result.exact.witnesses).toMatchObject({
      requiredKeyHashes: [credential],
      requestedDeviceKeyHashes: [credential],
      missingKeyHashes: [],
      unexpectedKeyHashes: [],
    });
    expect(result.status === 'rejected' && result.reasons).toContain(
      'root-envelope:reject_pre_device'
    );
    for (const capability of [
      { ...rejectedCapability, artifactId: 'ledger-7.1.4' },
      rejectedCapability,
      {
        ...rejectedCapability,
        vendor: 'trezor' as const,
        artifactId: 'trezor-connect-9.7.2',
        rowId: 'trezor-signTx',
      },
    ]) {
      const current = prepareHardwareTransaction(
        snapshot(),
        0,
        false,
        capability
      );
      expect(current.status).toBe('rejected');
      expect(current.deviceInteraction).toBe(false);
    }
  });

  it('never derives a reference signer and returns partial empty before gating', () => {
    const result = prepareHardwareTransaction(
      snapshot(false),
      0,
      true,
      rejectedCapability
    );
    expect(result).toMatchObject({
      status: 'empty',
      deviceInteraction: false,
      witnessSetCbor: 'a0',
      exact: {
        signers: [],
        witnesses: {
          requestedDeviceKeyHashes: [],
          missingKeyHashes: [credential],
        },
      },
    });
  });

  it('produces a vendor-neutral ready model only for an explicit test gate', () => {
    const result = prepareHardwareTransaction(
      snapshot(),
      0,
      true,
      readyCapability
    );
    expect(result).toMatchObject({ status: 'ready', deviceInteraction: true });
    expect(result.exact).not.toHaveProperty('coinSelection');
    expect(result.exact).not.toHaveProperty('vendorRequest');
  });

  it('verifies the exact expected hardware witness set and rejects drift', () => {
    const prepared = prepareHardwareTransaction(
      snapshot(),
      0,
      false,
      readyCapability
    );
    if (prepared.status !== 'ready') throw new Error('Expected ready model');
    const signature = sign(
      null,
      Buffer.from(prepared.exact.bodyHash, 'hex'),
      keys.privateKey
    ).toString('hex');
    const response = {
      bodyHash: prepared.exact.bodyHash,
      witnesses: [{ publicKey: publicKey.toString('hex'), signature }],
    };
    expect(
      verifyHardwareTransactionWitnesses(prepared.exact, response)
    ).not.toBe('a0');
    for (const changed of [
      { ...response, bodyHash: '00'.repeat(32) },
      { ...response, witnesses: [] },
      {
        ...response,
        witnesses: [...response.witnesses, ...response.witnesses],
      },
      {
        ...response,
        witnesses: [{ ...response.witnesses[0], signature: '00'.repeat(64) }],
      },
    ])
      expect(() =>
        verifyHardwareTransactionWitnesses(prepared.exact, changed)
      ).toThrow(WitnessSetError);
  });
});
