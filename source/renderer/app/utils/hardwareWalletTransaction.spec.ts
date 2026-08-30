import fs from 'fs';
import path from 'path';
import { generateKeyPairSync, sign } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import type {
  ContextOutput,
  TransactionContextSnapshot,
} from '../../../common/cardano/transactionContext';
import type {
  HardwareExactTransaction,
  HardwareTransactionCapability,
} from '../../../common/types/hardware-wallets.types';
import {
  verifyHardwareTransactionWitnesses,
  WitnessSetError,
} from '../../../common/cardano/witnessSet';
import { preflightCip103Sign } from '../domains/Cip103Batch';
import { prepareHardwareTransaction } from './hardwareWalletTransaction';
import { toExactLedgerSignTransactionRequest } from './shelleyLedger';
import {
  assertExactTrezorBody,
  toExactTrezorSignTransactionRequest,
} from '../../../common/hardware/trezorTransaction';

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
  pendingTransactions: [],
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
  physicalCertified: true,
  productEnabled: true,
  familyDispositions: { 'root-envelope': 'representable' },
};

const ledgerReadyCapability: HardwareTransactionCapability = {
  ...readyCapability,
  artifactId: 'ledger-8.0.0-candidate',
};
const trezorReadyCapability: HardwareTransactionCapability = {
  ...readyCapability,
  artifactId: 'trezor-connect-9.7.2',
  vendor: 'trezor',
  rowId: 'trezor-signTx',
};

const exactTransaction = (cborHex: string): HardwareExactTransaction => {
  const preflight = preflightCip103Sign([{ cbor: cborHex }], 0).items[0];
  return {
    transaction: preflight.transaction,
    bodyHash: preflight.bodyHash,
    contextDigest: 'cc'.repeat(32),
    network: { networkId: 0, networkMagic: 42, genesisHash: 'bb'.repeat(32) },
    partialSign: true,
    signers: [],
    ownedInputs: [],
    ownedOutputs: [],
    witnesses: {
      requiredKeyHashes: [],
      preExistingKeyHashes: [],
      requestedDeviceKeyHashes: [],
      missingKeyHashes: [],
      unexpectedKeyHashes: [],
    },
    capability: ledgerReadyCapability,
  };
};

const exactTrezorTransaction = (cborHex: string): HardwareExactTransaction => ({
  ...exactTransaction(cborHex),
  capability: trezorReadyCapability,
});

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
    expect(result.exact.ownedInputs).toEqual([
      {
        transactionId: normalId,
        index: BigInt(0),
        path: [0x8000073c, 0x80000717, 0x80000000, 0, 0],
        role: 'normal',
      },
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

  it.each([
    ['physicalCertified', 'physical-certification'],
    ['productEnabled', 'product-disabled'],
  ] as const)('rejects %s before device interaction', (gate, reason) => {
    const result = prepareHardwareTransaction(snapshot(), 0, true, {
      ...readyCapability,
      [gate]: false,
    });
    expect(result).toMatchObject({
      status: 'rejected',
      deviceInteraction: false,
      reasons: expect.arrayContaining([reason]),
    });
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

  it('maps the immutable body with its owned input and unowned reference', () => {
    const result = prepareHardwareTransaction(
      snapshot(),
      0,
      false,
      ledgerReadyCapability
    );
    if (result.status !== 'ready') throw new Error('Expected ready model');
    const request = toExactLedgerSignTransactionRequest(result.exact);
    expect(request.tx.inputs).toEqual([
      expect.objectContaining({
        txHashHex: normalId,
        outputIndex: 0,
        path: [0x8000073c, 0x80000717, 0x80000000, 0, 0],
      }),
    ]);
    expect(request.tx.referenceInputs).toEqual([
      expect.objectContaining({
        txHashHex: referenceId,
        outputIndex: 0,
        path: null,
      }),
    ]);
  });

  it('proves canonical Plutus reconstruction and rejects the locked noncanonical fixture', () => {
    const input = [Buffer.from(normalId, 'hex'), 0];
    const collateral = [Buffer.from('44'.repeat(32), 'hex'), 0];
    const reference = [Buffer.from(referenceId, 'hex'), 0];
    const output = [Buffer.from(address, 'hex'), 900_000];
    const plutusBody = new Map<number, unknown>([
      [0, new cbor.Tagged(258, [input])],
      [1, [output]],
      [2, 100_000],
      [13, new cbor.Tagged(258, [collateral])],
      [16, output],
      [17, 100_000],
      [18, new cbor.Tagged(258, [reference])],
    ]);
    const plutus = cbor
      .encodeCanonical([plutusBody, new Map(), true, null])
      .toString('hex');
    const request = toExactLedgerSignTransactionRequest(
      exactTransaction(plutus)
    );
    expect(request.tx).toMatchObject({
      collateralInputs: expect.any(Array),
      collateralOutput: expect.any(Object),
      totalCollateral: expect.any(String),
    });
    expect(request.signingMode).toBe('plutus_transaction');
    expect(request.options).toEqual({ tagCborSets: true });

    const fixture = JSON.parse(
      fs.readFileSync(
        path.join(
          __dirname,
          '../../../common/cardano/fixtures/exact-cbor/conway-regression.json'
        ),
        'utf8'
      )
    );
    expect(() =>
      toExactLedgerSignTransactionRequest(exactTransaction(fixture.cborHex))
    ).toThrow(/body reconstruction/u);
  });
  it('maps frozen ordinary, Plutus, and supported Conway Trezor fields', () => {
    const ordinaryBody = new Map<number, unknown>([
      [0, [[Buffer.from(normalId, 'hex'), 0]]],
      [1, [[Buffer.from(address, 'hex'), 900_000]]],
      [2, 100_000],
      [4, [[7, [0, Buffer.from(credential, 'hex')], 2_000_000]]],
      [8, 10],
      [15, 0],
    ]);
    const ordinary = cbor
      .encodeCanonical([ordinaryBody, new Map(), true, null])
      .toString('hex');
    const ordinaryExact = exactTrezorTransaction(ordinary);
    const ordinaryRequest = toExactTrezorSignTransactionRequest(ordinaryExact);
    expect(ordinaryRequest).toMatchObject({
      certificates: [{ type: 7, keyHash: credential, deposit: '2000000' }],
      validityIntervalStart: '10',
      includeNetworkId: true,
    });
    const ordinaryOutput = ordinaryRequest.outputs[0];
    if (!('address' in ordinaryOutput))
      throw new Error('Expected external Trezor output');
    expect(ordinaryOutput.address).toMatch(/^addr_test1/u);
    expect(() =>
      assertExactTrezorBody(ordinaryExact, {
        ...ordinaryRequest,
        fee: '1',
      })
    ).toThrow(/body reconstruction/u);

    const plutusBody = new Map<number, unknown>([
      [0, new cbor.Tagged(258, [[Buffer.from(normalId, 'hex'), 0]])],
      [1, [[Buffer.from(address, 'hex'), 900_000]]],
      [2, 100_000],
      [11, Buffer.from('33'.repeat(32), 'hex')],
      [13, new cbor.Tagged(258, [[Buffer.from('44'.repeat(32), 'hex'), 0]])],
      [16, [Buffer.from(address, 'hex'), 900_000]],
      [17, 100_000],
      [18, new cbor.Tagged(258, [[Buffer.from(referenceId, 'hex'), 0]])],
    ]);
    const plutus = cbor
      .encodeCanonical([plutusBody, new Map(), true, null])
      .toString('hex');
    expect(
      toExactTrezorSignTransactionRequest(exactTrezorTransaction(plutus))
    ).toMatchObject({
      signingMode: 3,
      scriptDataHash: '33'.repeat(32),
      collateralInputs: expect.any(Array),
      collateralReturn: expect.any(Object),
      referenceInputs: expect.any(Array),
    });
    const mixedSets = new Map(plutusBody);
    mixedSets.set(0, [[Buffer.from(normalId, 'hex'), 0]]);
    const mixed = cbor
      .encodeCanonical([mixedSets, new Map(), true, null])
      .toString('hex');
    expect(() =>
      toExactTrezorSignTransactionRequest(exactTrezorTransaction(mixed))
    ).toThrow(/mixed set tagging/u);
  });

  it('maps the complete frozen Trezor field and certificate inventory', () => {
    const paymentCredential = [0, Buffer.from(credential, 'hex')];
    const baseEntries: Array<[number, unknown]> = [
      [0, [[Buffer.from(normalId, 'hex'), 0]]],
      [1, [[Buffer.from(address, 'hex'), 900_000]]],
      [2, 100_000],
    ];
    const requestFor = (entries: Array<[number, unknown]>) => {
      const body = new Map<number, unknown>(baseEntries);
      entries.forEach(([key, value]) => body.set(key, value));
      return toExactTrezorSignTransactionRequest(
        exactTrezorTransaction(
          cbor.encodeCanonical([body, new Map(), true, null]).toString('hex')
        )
      );
    };
    const rewardAccount = Buffer.from(`e0${credential}`, 'hex');
    const policyId = Buffer.from('55'.repeat(28), 'hex');
    const fieldsRequest = requestFor([
      [3, 500],
      [5, new Map([[rewardAccount, 7]])],
      [8, 10],
      [9, new Map([[policyId, new Map([[Buffer.from('01', 'hex'), 2]])]])],
      [14, [Buffer.from(credential, 'hex')]],
      [15, 0],
    ]);
    expect(fieldsRequest).toMatchObject({
      ttl: '500',
      withdrawals: [{ keyHash: credential, amount: '7' }],
      validityIntervalStart: '10',
      mint: [
        {
          policyId: '55'.repeat(28),
          tokenAmounts: [{ assetNameBytes: '01', mintAmount: '2' }],
        },
      ],
      requiredSigners: [{ keyHash: credential }],
      includeNetworkId: true,
    });

    const supportedCertificates: Array<[number, unknown[], object]> = [
      [0, [paymentCredential], { type: 0, keyHash: credential }],
      [1, [paymentCredential], { type: 1, keyHash: credential }],
      [
        2,
        [paymentCredential, Buffer.from('66'.repeat(28), 'hex')],
        { type: 2, keyHash: credential, pool: '66'.repeat(28) },
      ],
      [
        7,
        [paymentCredential, 2_000_000],
        { type: 7, keyHash: credential, deposit: '2000000' },
      ],
      [
        8,
        [paymentCredential, 2_000_000],
        { type: 8, keyHash: credential, deposit: '2000000' },
      ],
      [9, [paymentCredential, [2]], { type: 9, dRep: { type: 2 } }],
    ];
    supportedCertificates.forEach(([tag, parts, expected]) => {
      expect(requestFor([[4, [[tag, ...parts]]]])).toMatchObject({
        certificates: [expected],
      });
    });

    const poolCertificate = [
      3,
      [
        Buffer.from('77'.repeat(28), 'hex'),
        Buffer.from('88'.repeat(32), 'hex'),
        1,
        2,
        new cbor.Tagged(30, [1, 2]),
        rewardAccount,
        new cbor.Tagged(258, [Buffer.from(credential, 'hex')]),
        [
          [
            0,
            3000,
            Buffer.from([192, 0, 2, 1]),
            Buffer.from('20010db8000000000000000000000001', 'hex'),
          ],
        ],
        null,
      ],
    ];
    const poolBody = new Map<number, unknown>([
      [0, new cbor.Tagged(258, [[Buffer.from(normalId, 'hex'), 0]])],
      [1, [[Buffer.from(address, 'hex'), 900_000]]],
      [2, 100_000],
      [4, new cbor.Tagged(258, [poolCertificate])],
    ]);
    const poolParsed = exactTrezorTransaction(
      cbor.encodeCanonical([poolBody, new Map(), true, null]).toString('hex')
    );
    const poolExact: HardwareExactTransaction = {
      ...poolParsed,
      signers: [
        {
          credentialKind: 'stake',
          keyHash: credential,
          path: [0x8000073c, 0x80000717, 0x80000000, 2, 0],
          proofKinds: ['certificate'],
        },
      ],
      witnesses: {
        requiredKeyHashes: [credential],
        preExistingKeyHashes: [],
        requestedDeviceKeyHashes: [credential],
        missingKeyHashes: [],
        unexpectedKeyHashes: [],
      },
    };
    expect(toExactTrezorSignTransactionRequest(poolExact)).toMatchObject({
      signingMode: 1,
      certificates: [
        {
          type: 3,
          poolParameters: {
            owners: [{ stakingKeyPath: expect.any(Array) }],
            relays: [
              {
                ipv4Address: '192.0.2.1',
                ipv6Address: '2001:0db8:0000:0000:0000:0000:0000:0001',
              },
            ],
          },
        },
      ],
    });

    const mapOutputRequest = requestFor([
      [
        1,
        [
          new Map<number, unknown>([
            [0, Buffer.from(address, 'hex')],
            [1, 900_000],
          ]),
        ],
      ],
    ]);
    expect(mapOutputRequest.outputs).toEqual([
      expect.objectContaining({ format: 1 }),
    ]);
  });

  it('rejects locked Trezor governance and unsupported certificate boundaries', () => {
    const exact = exactTrezorTransaction(transactionCbor);
    for (const key of [19, 20, 21, 22]) {
      const transaction = {
        ...exact.transaction,
        governance: {
          ...exact.transaction.governance,
          ...(key === 19 ? { votes: [{}] } : {}),
          ...(key === 20
            ? {
                proposals: [
                  {
                    value: '',
                    decoded: { kind: 'array', items: [] },
                    span: { start: 0, end: 0 },
                    policyScriptHashes: [],
                  },
                ],
              }
            : {}),
          ...(key === 21 ? { treasuryValue: BigInt(1) } : {}),
          ...(key === 22 ? { donation: BigInt(1) } : {}),
        },
      };
      expect(() =>
        toExactTrezorSignTransactionRequest({
          ...exact,
          transaction: transaction as HardwareExactTransaction['transaction'],
        })
      ).toThrow(/governance/u);
    }
    const stake = [0, Buffer.from(credential, 'hex')];
    const pool = Buffer.from('44'.repeat(28), 'hex');
    const unsupportedCertificates: Record<number, unknown[]> = {
      4: [pool, 1],
      10: [stake, pool, [2]],
      11: [stake, pool, 1],
      12: [stake, [2], 1],
      13: [stake, pool, [2], 1],
      14: [stake, stake],
      15: [stake, null],
      16: [stake, 1, null],
      17: [stake, 1],
      18: [stake, null],
    };
    for (const [tag, parts] of Object.entries(unsupportedCertificates)) {
      const unsupportedBody = new Map<number, unknown>([
        [0, [[Buffer.from(normalId, 'hex'), 0]]],
        [1, [[Buffer.from(address, 'hex'), 900_000]]],
        [2, 100_000],
        [4, [[Number(tag), ...parts]]],
      ]);
      const unsupported = cbor
        .encodeCanonical([unsupportedBody, new Map(), true, null])
        .toString('hex');
      expect(() =>
        toExactTrezorSignTransactionRequest(exactTrezorTransaction(unsupported))
      ).toThrow(/certificate/u);
    }
  });

  it('rejects non-reconstructible and proposal bodies before device use', () => {
    const nonCanonicalBody = new Map<number, unknown>([
      [2, 100_000],
      [0, [[Buffer.from(normalId, 'hex'), 0]]],
      [1, [[Buffer.from(address, 'hex'), 900_000]]],
    ]);
    const nonCanonical = cbor
      .encode([nonCanonicalBody, new Map(), true, null])
      .toString('hex');
    expect(() =>
      toExactLedgerSignTransactionRequest(exactTransaction(nonCanonical))
    ).toThrow(/body reconstruction/u);

    const exact = exactTransaction(transactionCbor);
    const proposal: HardwareExactTransaction = {
      ...exact,
      transaction: {
        ...exact.transaction,
        governance: {
          ...exact.transaction.governance,
          proposals: [
            {
              value: '',
              decoded: { kind: 'array', items: [] },
              span: { start: 0, end: 0 },
              policyScriptHashes: [],
            },
          ],
        },
      },
    };
    expect(() => toExactLedgerSignTransactionRequest(proposal)).toThrow(
      /proposal procedure/u
    );
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
