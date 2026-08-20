import fs from 'fs';
import path from 'path';
import { createHmac } from 'crypto';
import { blake2b } from 'blakejs';

type Outpoint = { transactionId: string; index: number };

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

test('reproduces backend context records, digest, and token', () => {
  expect(fixture.source.candidateCommit).toBe(
    '3ca15553f96587f1f96688185165b2ede00e30b0'
  );
  const { fullOutput, protocol, pending } = fixture.records;
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
  expect(pendingRecord.toString('hex')).toBe(pending.encoded);

  const digestInput = fixture.digest;
  const encodedRecords = {
    fullOutput: fullOutputRecord,
    protocol: protocolRecord,
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
