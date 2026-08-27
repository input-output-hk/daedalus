import { generateKeyPairSync, sign } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import {
  computeContextDigest,
  ContextExpectation,
  reconcileTransactionContext,
  validateContextRecords,
} from './transactionContext';

const walletId = 'ab'.repeat(20);
const genesisHash = '01'.repeat(32);
const blockHash = '02'.repeat(32);
const address = Buffer.from(`60${'aa'.repeat(28)}`, 'hex');
const output = cbor.encodeCanonical([address, 1_000_000]).toString('hex');
const u8 = (value: number) => Buffer.from([value]);
const u32 = (value: number) => {
  const result = Buffer.alloc(4);
  result.writeUInt32BE(value);
  return result;
};
const u64 = (value: bigint) => {
  const result = Buffer.alloc(8);
  result.writeBigUInt64BE(value);
  return result;
};
const sized = (value: Buffer) => Buffer.concat([u32(value.length), value]);
const vector = (values: Buffer[]) =>
  Buffer.concat([u32(values.length), ...values.map(sized)]);
const record = (tag: number, body: Buffer) =>
  Buffer.concat([u8(tag), sized(body)]).toString('hex');
const outpoint = (transactionId: string, index = 0) =>
  Buffer.concat([Buffer.from(transactionId, 'hex'), u32(index)]);

const makeTransaction = (
  ids = ['11'.repeat(32), '22'.repeat(32), '33'.repeat(32)],
  witnessSet: Map<number, unknown> = new Map()
) => {
  const body = new Map<number, unknown>([
    [0, [[Buffer.from(ids[0], 'hex'), 0]]],
    [1, [[address, 900_000]]],
    [2, 100_000],
    [13, [[Buffer.from(ids[1], 'hex'), 0]]],
    [18, [[Buffer.from(ids[2], 'hex'), 0]]],
  ]);
  return {
    body,
    cbor: cbor.encodeCanonical([body, witnessSet, true, null]).toString('hex'),
    inputs: [
      { transactionId: ids[0], role: 'normal' },
      { transactionId: ids[1], role: 'collateral' },
      { transactionId: ids[2], role: 'reference' },
    ],
  };
};

const makeResponse = (
  transaction: ReturnType<typeof makeTransaction>,
  transactionOverride?: string,
  protocolParametersCbor = 'a0'
) => {
  const transactions = [transactionOverride || transaction.cbor];
  const expectation: ContextExpectation = {
    walletId,
    network: { networkId: 0, networkMagic: 42, genesisHash },
    transactions,
  };
  const outputs = transaction.inputs.map(({ transactionId, role }) => {
    const input = cbor
      .encodeCanonical([Buffer.from(transactionId, 'hex'), 0])
      .toString('hex');
    return {
      outpoint: { transaction_id: transactionId, index: 0 },
      transaction_input_cbor: input,
      source_transaction_output_cbor: output,
      canonical_transaction_output_cbor: output,
      transaction_unspent_output_cbor: cbor
        .encodeCanonical([
          [Buffer.from(transactionId, 'hex'), 0],
          cbor.decodeFirstSync(Buffer.from(output, 'hex')),
        ])
        .toString('hex'),
      provenance: ['node'],
      roles: [role],
      wallet_member: true,
      pending_state: 'none',
    };
  });
  const records = outputs.map((item) =>
    record(
      1,
      Buffer.concat([
        outpoint(item.outpoint.transaction_id),
        u8(4),
        u8(
          ({ normal: 1, collateral: 2, reference: 4 } as const)[item.roles[0]]
        ),
        u8(1),
        u8(0),
        sized(Buffer.from(output, 'hex')),
      ])
    )
  );
  records.push(
    record(
      3,
      Buffer.concat([
        sized(Buffer.from('conway')),
        u8(0),
        u32(42),
        u32(9),
        u32(0),
        sized(Buffer.from(protocolParametersCbor, 'hex')),
      ])
    )
  );
  records.sort();
  const point = { kind: 'block' as const, slot: BigInt(42), blockHash };
  const digest = computeContextDigest(
    expectation,
    point,
    BigInt(7),
    BigInt(9),
    records
  );
  const payload = Buffer.concat([
    u8(1),
    Buffer.alloc(16, 0x44),
    u32(1),
    sized(Buffer.from(walletId)),
    Buffer.from(genesisHash, 'hex'),
    Buffer.from(digest, 'hex'),
  ]);
  return {
    expectation,
    response: {
      revision: 1,
      wallet_id: walletId,
      network: { network_id: 0, network_magic: 42, genesis_hash: genesisHash },
      chain_point: { kind: 'block', slot: '42', block_hash: blockHash },
      wallet_generation: '7',
      pending_generation: '9',
      era: 'conway',
      protocol_version: { major: 9, minor: 0 },
      protocol_parameters_cbor: protocolParametersCbor,
      volatile_delta: {
        point: { kind: 'block', slot: '43', block_hash: '03'.repeat(32) },
        node_transaction_inputs: [],
      },
      outputs,
      pending_overlay: {
        transactions: [],
        spent_wallet_inputs: [],
        produced_wallet_outputs: [],
      },
      ownership: [],
      required_wallet_proofs: [],
      batch_overlay: { dependencies: [], conflicts: [] },
      records,
      context_digest: digest,
      context_token: Buffer.concat([payload, Buffer.alloc(32, 0x55)]).toString(
        'hex'
      ),
    },
  };
};

test('reconciles all input roles into one immutable trusted snapshot', () => {
  const fixture = makeResponse(
    makeTransaction(),
    undefined,
    cbor.encodeCanonical(new Map([[24, 3]])).toString('hex')
  );
  const snapshot = reconcileTransactionContext(
    fixture.response,
    fixture.expectation
  );
  expect(snapshot.transactionsSemantic[0].review).toEqual({
    complete: true,
    signable: true,
    requirements: [],
  });
  expect(snapshot.commitmentContexts[0].resolvedInputs).toHaveLength(3);
  expect(snapshot.walletGeneration).toBe(BigInt(7));
  expect(snapshot.maxCollateralInputs).toBe(3);
  expect(Object.isFrozen(snapshot)).toBe(true);
});

test('fails closed on identity, digest, token, record, and output mutations', () => {
  const fixture = makeResponse(makeTransaction());
  const mutations = [
    { wallet_id: 'cd'.repeat(20) },
    {
      network: {
        ...fixture.response.network,
        genesis_hash: '09'.repeat(32),
      },
    },
    { context_digest: '00'.repeat(32) },
    {
      context_token: `${fixture.response.context_token.slice(
        0,
        -66
      )}${'00'.repeat(33)}`,
    },
    {
      records: [
        ...fixture.response.records,
        fixture.response.records[0],
      ].sort(),
    },
    {
      outputs: fixture.response.outputs.map((item, index) =>
        index ? item : { ...item, wallet_member: false }
      ),
    },
  ];
  mutations.forEach((mutation) =>
    expect(() =>
      reconcileTransactionContext(
        { ...fixture.response, ...mutation },
        fixture.expectation
      )
    ).toThrow()
  );
});

test('validates every frozen context record tag and rejects malformed records', () => {
  const credential = Buffer.alloc(28, 0xaa);
  const records = [
    record(
      1,
      Buffer.concat([
        outpoint('11'.repeat(32)),
        u8(4),
        u8(1),
        u8(1),
        u8(0),
        sized(Buffer.from(output, 'hex')),
      ])
    ),
    record(2, Buffer.concat([u8(1), sized(credential), u8(0), u32(0), u32(0)])),
    record(
      3,
      Buffer.concat([
        sized(Buffer.from('conway')),
        u8(0),
        u32(42),
        u32(9),
        u32(0),
        sized(Buffer.from('a0', 'hex')),
      ])
    ),
    record(4, Buffer.concat([u8(2), sized(credential), u8(2)])),
    record(
      5,
      Buffer.concat([
        u8(3),
        sized(credential),
        u8(1),
        u8(2),
        u64(BigInt(500)),
        u8(0),
      ])
    ),
    record(6, Buffer.concat([u32(0), u8(1), u8(1), sized(credential), u8(1)])),
    record(
      7,
      Buffer.concat([
        Buffer.alloc(32, 0xbb),
        u8(4),
        sized(Buffer.from('84a0a0f5f6', 'hex')),
        vector([outpoint('11'.repeat(32))]),
        vector([]),
        u8(0),
      ])
    ),
  ].sort();
  expect(() => validateContextRecords(records)).not.toThrow();
  expect(() => validateContextRecords([record(8, Buffer.alloc(0))])).toThrow(
    'unknown context record tag'
  );
  expect(() => validateContextRecords([records[0].slice(0, -2)])).toThrow();
  expect(() => validateContextRecords([records[0], records[0]])).toThrow(
    'duplicate context record'
  );
});

test('verifies and labels existing witnesses against the exact body hash', () => {
  const { publicKey, privateKey } = generateKeyPairSync('ed25519');
  const rawPublicKey = publicKey
    .export({ format: 'der', type: 'spki' })
    .slice(-32);
  const unsigned = makeTransaction();
  const bodyHash = Buffer.from(
    blake2b(cbor.encodeCanonical(unsigned.body), undefined, 32)
  );
  const signature = sign(null, bodyHash, privateKey);
  const witness = cbor.encodeCanonical([rawPublicKey, signature]);
  const signed = makeTransaction(
    unsigned.inputs.map(({ transactionId }) => transactionId),
    new Map([[0, [[rawPublicKey, signature]]]])
  );
  const fixture = makeResponse(signed);
  const snapshot = reconcileTransactionContext(
    fixture.response,
    fixture.expectation
  );
  expect(snapshot.preExistingWitnesses).toEqual([
    { transactionIndex: 0, kind: 'vkey', cbor: witness.toString('hex') },
  ]);
  expect(snapshot.outputs).toHaveLength(3);
  expect(snapshot.outputs[0]).toMatchObject({
    outpoint: { transactionId: signed.inputs[0].transactionId, index: 0 },
    canonicalCbor: output,
    walletMember: true,
  });
  expect(Object.isFrozen(snapshot.outputs)).toBe(true);

  const invalidSignature = Buffer.from(signature);
  invalidSignature[0] ^= 1;
  const tampered = makeTransaction(
    unsigned.inputs.map(({ transactionId }) => transactionId),
    new Map([[0, [[rawPublicKey, invalidSignature]]]])
  );
  const badFixture = makeResponse(tampered);
  expect(() =>
    reconcileTransactionContext(badFixture.response, badFixture.expectation)
  ).toThrow('invalid existing witness signature');
});
