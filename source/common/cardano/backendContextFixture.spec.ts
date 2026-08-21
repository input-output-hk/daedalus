import fs from 'fs';
import path from 'path';
import { createHmac } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

type Outpoint = { transactionId: string; index: number };
type InputRole = 'normal' | 'collateral' | 'reference';

const fixture = JSON.parse(
  fs.readFileSync(
    path.join(__dirname, 'fixtures', 'exact-cbor', 'backend-context-v1.json'),
    'utf8'
  )
);

const hex = (value: string) => Buffer.from(value, 'hex');
const u8 = (value: number) => Buffer.from([value]);
const u32 = (value: number) => {
  const result = Buffer.alloc(4);
  result.writeUInt32BE(value);
  return result;
};
const u64 = (value: string) => {
  const result = Buffer.alloc(8);
  result.writeBigUInt64BE(BigInt(value));
  return result;
};
const bytes = (value: Buffer) => Buffer.concat([u32(value.length), value]);
const vector = (values: Buffer[]) =>
  Buffer.concat([u32(values.length), ...values.map(bytes)]);
const outpoint = (value: Outpoint) =>
  Buffer.concat([hex(value.transactionId), u32(value.index)]);
const record = (tag: number, body: Buffer) =>
  Buffer.concat([u8(tag), bytes(body)]);
const transactionId = (body: Map<number, unknown>) =>
  Buffer.from(blake2b(cbor.encodeCanonical(body), undefined, 32)).toString(
    'hex'
  );

test('reproduces backend context records, digest, and token', () => {
  expect(fixture.source.candidateCommit).toBe(
    'e60b8a66cad9121e54656c76e03a3785099b9215'
  );
  const {
    fullOutput,
    ownership,
    protocol,
    requiredProof,
    pending,
  } = fixture.records;
  const fullOutputRecord = record(
    1,
    Buffer.concat([
      outpoint(fullOutput),
      u8(fullOutput.provenanceBits),
      u8(fullOutput.roleBits),
      u8(Number(fullOutput.walletMember)),
      u8(fullOutput.pendingState),
      bytes(hex(fullOutput.exactLedgerTxOutCbor)),
    ])
  );
  const protocolRecord = record(
    3,
    Buffer.concat([
      bytes(Buffer.from('conway')),
      u8(protocol.networkId),
      u32(protocol.networkMagic),
      u32(protocol.protocolMajor),
      u32(protocol.protocolMinor),
      bytes(hex(protocol.protocolParametersCbor)),
    ])
  );
  const ownershipRecord = record(
    2,
    Buffer.concat([
      u8(ownership.credentialKind),
      bytes(hex(ownership.credential)),
      u8(ownership.ownership),
      u32(ownership.derivationPath.length),
      ...ownership.derivationPath.map(u32),
      u32(ownership.proofBits),
    ])
  );
  const requiredProofRecord = record(
    6,
    Buffer.concat([
      u32(requiredProof.transactionIndex),
      u8(requiredProof.proofKind),
      u8(requiredProof.credentialKind),
      bytes(hex(requiredProof.credential)),
      u8(Number(requiredProof.required)),
    ])
  );
  const pendingRecord = record(
    7,
    Buffer.concat([
      hex(pending.transactionId),
      u8(4),
      bytes(hex(pending.exactSealedTransaction)),
      vector(pending.normalInputs.map(outpoint)),
      vector(pending.collateralInputs.map(outpoint)),
      u8(1),
      u64(pending.expirySlot),
    ])
  );

  expect(fullOutputRecord.toString('hex')).toBe(fullOutput.encoded);
  expect(protocolRecord.toString('hex')).toBe(protocol.encoded);
  expect(ownershipRecord.toString('hex')).toBe(ownership.encoded);
  expect(requiredProofRecord.toString('hex')).toBe(requiredProof.encoded);
  expect(pendingRecord.toString('hex')).toBe(pending.encoded);
  const digestInput = fixture.digest;
  const transactions = digestInput.transactions.map((encoded: string) => {
    const exact = hex(encoded);
    const decoded = cbor.decodeFirstSync(exact) as [
      Map<number, unknown>,
      unknown,
      boolean,
      unknown
    ];
    expect(cbor.encodeCanonical(decoded)).toEqual(exact);
    expect(decoded[2]).toBe(true);
    return { body: decoded[0], id: transactionId(decoded[0]) };
  });
  expect(transactions).toHaveLength(3);
  expect(fixture.validityBoundary.accepted).toBe(digestInput.transactions[0]);
  expect(
    (cbor.decodeFirstSync(
      hex(fixture.validityBoundary.rejected)
    ) as unknown[])[2]
  ).toBe(false);

  const dependencies = [];
  const conflicts = [];
  const produced = new Map<string, number>();
  const consumed = new Map<string, number>();
  transactions.forEach(({ body, id }, transactionIndex) => {
    const roleInputs: [InputRole, [Buffer, number][]][] = [
      ['normal', (body.get(0) as [Buffer, number][]) || []],
      ['collateral', (body.get(13) as [Buffer, number][]) || []],
      ['reference', (body.get(18) as [Buffer, number][]) || []],
    ];
    roleInputs.forEach(([inputRole, inputs]) => {
      inputs.forEach(([inputId, index]) => {
        const value = { transactionId: inputId.toString('hex'), index };
        const key = `${value.transactionId}#${index}`;
        const sourceTransactionIndex = produced.get(key);
        if (sourceTransactionIndex !== undefined) {
          dependencies.push({
            transactionIndex,
            inputRole,
            outpoint: value,
            source: 'earlier',
            sourceTransactionIndex,
          });
        }
        const earlierTransactionIndex = consumed.get(key);
        if (
          earlierTransactionIndex !== undefined &&
          inputRole !== 'reference'
        ) {
          conflicts.push({
            transactionIndex,
            inputRole,
            outpoint: value,
            earlierTransactionIndex,
          });
        }
      });
    });
    roleInputs[0][1].forEach(([inputId, index]) => {
      const key = `${inputId.toString('hex')}#${index}`;
      if (!consumed.has(key)) consumed.set(key, transactionIndex);
    });
    ((body.get(1) as unknown[]) || []).forEach((_, index) =>
      produced.set(`${id}#${index}`, transactionIndex)
    );
  });
  expect(dependencies).toEqual(fixture.batchOverlay.dependencies);
  expect(conflicts).toEqual(fixture.batchOverlay.conflicts);
  expect(fullOutput.transactionId).toBe(transactions[0].id);
  expect(fullOutput.exactLedgerTxOutCbor).toBe(
    cbor
      .encodeCanonical(
        (transactions[0].body.get(1) as unknown[])[fullOutput.index]
      )
      .toString('hex')
  );
  const [outputAddress] = cbor.decodeFirstSync(
    hex(fullOutput.exactLedgerTxOutCbor)
  ) as [Buffer, number];
  expect(ownership.credential).toBe(outputAddress.subarray(1).toString('hex'));
  expect(requiredProof.transactionIndex).toBe(
    fixture.batchOverlay.dependencies[0].transactionIndex
  );
  expect(requiredProof.credential).toBe(ownership.credential);

  const encodedRecords = {
    fullOutput: fullOutputRecord,
    ownership: ownershipRecord,
    protocol: protocolRecord,
    requiredProof: requiredProofRecord,
    pending: pendingRecord,
  };
  const sortedRecords = digestInput.records
    .map((name: keyof typeof encodedRecords) => encodedRecords[name])
    .sort(Buffer.compare);
  const chainPoint = Buffer.concat([
    u8(1),
    u64(digestInput.chainPoint.slot),
    bytes(hex(digestInput.chainPoint.blockHash)),
  ]);
  const digest = Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from('daedalus-dapp-context-v1'),
        bytes(Buffer.from(digestInput.walletId)),
        bytes(hex(digestInput.genesisHash)),
        chainPoint,
        u64(digestInput.walletGeneration),
        u64(digestInput.pendingGeneration),
        vector(digestInput.transactions.map(hex)),
        vector(sortedRecords),
      ]),
      undefined,
      32
    )
  );
  expect(digest.toString('hex')).toBe(digestInput.encoded);

  const token = fixture.token;
  const payload = Buffer.concat([
    u8(1),
    hex(token.processGeneration),
    u32(token.capabilityRevision),
    bytes(Buffer.from(digestInput.walletId)),
    hex(digestInput.genesisHash),
    digest,
  ]);
  const mac = createHmac('sha256', hex(token.key))
    .update(
      Buffer.concat([
        Buffer.from('daedalus-dapp-context-token-v1'),
        bytes(payload),
      ])
    )
    .digest();
  expect(Buffer.concat([payload, mac]).toString('hex')).toBe(token.encoded);
});
