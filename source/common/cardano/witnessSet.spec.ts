import { generateKeyPairSync, KeyObject, sign } from 'crypto';
import cbor from 'cbor';

import { bytesForSpan, parseCborItem } from './cborSlices';
import { parseConwayTransactionEnvelope } from './transactionEnvelope';
import {
  diffVKeyWitnesses,
  encodeVKeyWitnessSet,
  extractVKeyWitnesses,
  mergeVKeyWitnesses,
  verifyVKeyWitnesses,
  WitnessSetError,
} from './witnessSet';

const body = new Map<number, unknown>([
  [0, []],
  [1, []],
  [2, 0],
]);

const envelopeWith = (witnessSet: Map<number, unknown> = new Map()) =>
  parseConwayTransactionEnvelope(
    cbor.encodeCanonical([body, witnessSet, true, null])
  );

const keyPair = () => {
  const pair = generateKeyPairSync('ed25519');
  return {
    privateKey: pair.privateKey,
    publicKey: pair.publicKey
      .export({ format: 'der', type: 'spki' })
      .subarray(-32),
  };
};

const witness = (
  privateKey: KeyObject,
  publicKey: Buffer,
  transactionId: string
) => ({
  publicKey,
  signature: sign(null, Buffer.from(transactionId, 'hex'), privateKey),
});

const pair = ({ publicKey, signature }: ReturnType<typeof witness>) => [
  publicKey,
  signature,
];

const rawFields = (source: Buffer) => {
  const envelope = parseConwayTransactionEnvelope(source);
  return new Map(
    (envelope.witnessSet.entries || []).map(({ key, value }) => [
      Number(key.value),
      source.subarray(key.span.start, value.span.end).toString('hex'),
    ])
  );
};

test('extracts, hashes, and canonically encodes VKey witnesses', () => {
  const envelope = envelopeWith();
  const keys = keyPair();
  const signed = witness(
    keys.privateKey,
    keys.publicKey,
    envelope.transactionId
  );
  const encoded = encodeVKeyWitnessSet([signed]);
  const [decoded] = extractVKeyWitnesses(encoded);

  expect(decoded.publicKey).toEqual(signed.publicKey);
  expect(decoded.signature).toEqual(signed.signature);
  expect(decoded.keyHash).toHaveLength(28);
  expect(encodeVKeyWitnessSet([]).toString('hex')).toBe('a0');
  expect(encodeVKeyWitnessSet([decoded])).toEqual(encoded);

  const tagged = cbor.encodeCanonical(
    new Map([[0, new cbor.Tagged(258, [pair(signed)])]])
  );
  expect(encodeVKeyWitnessSet(extractVKeyWitnesses(tagged))).toEqual(encoded);
});

test('rejects malformed, ambiguous, or non-VKey witness sets', () => {
  const keys = keyPair();
  const signature = Buffer.alloc(64);
  const validPair = [keys.publicKey, signature];
  const duplicateMapKey = Buffer.from('a2008100008100', 'hex');
  const chunkedKey = Buffer.concat([
    Buffer.from('a10081825f5820', 'hex'),
    keys.publicKey,
    Buffer.from('ff5840', 'hex'),
    signature,
  ]);
  const cases = [
    Buffer.from('80', 'hex'),
    Buffer.concat([cbor.encodeCanonical(new Map()), Buffer.from([0])]),
    cbor.encodeCanonical(new Map([[1, [validPair]]])),
    cbor.encodeCanonical(new Map([[0, []]])),
    cbor.encodeCanonical(new Map([[0, [[Buffer.alloc(31), signature]]]])),
    cbor.encodeCanonical(new Map([[0, [[keys.publicKey, Buffer.alloc(63)]]]])),
    cbor.encodeCanonical(new Map([[0, [[keys.publicKey, signature, 0]]]])),
    cbor.encodeCanonical(new Map([[0, [new cbor.Tagged(1, validPair)]]])),
    cbor.encodeCanonical(new Map([[0, [validPair, validPair]]])),
    duplicateMapKey,
    chunkedKey,
  ];

  cases.forEach((candidate) =>
    expect(() => extractVKeyWitnesses(candidate)).toThrow(WitnessSetError)
  );
});

test('verifies every signature over the exact transaction body hash', () => {
  const envelope = envelopeWith();
  const keys = keyPair();
  const signed = witness(
    keys.privateKey,
    keys.publicKey,
    envelope.transactionId
  );
  const bodyBytes = bytesForSpan(envelope.cbor, envelope.spans.body);

  expect(() => verifyVKeyWitnesses(bodyBytes, [signed])).not.toThrow();
  const changedSignature = Buffer.from(signed.signature);
  changedSignature[0] ^= 1;
  expect(() =>
    verifyVKeyWitnesses(bodyBytes, [
      { publicKey: signed.publicKey, signature: changedSignature },
    ])
  ).toThrow(WitnessSetError);
  expect(() =>
    verifyVKeyWitnesses(Buffer.concat([bodyBytes, Buffer.from([0])]), [signed])
  ).toThrow(WitnessSetError);
});

test('returns only fresh VKeys and preserves exact immutable fields on merge', () => {
  const unsigned = envelopeWith();
  const oldKeys = keyPair();
  const freshKeys = keyPair();
  const oldWitness = witness(
    oldKeys.privateKey,
    oldKeys.publicKey,
    unsigned.transactionId
  );
  const freshWitness = witness(
    freshKeys.privateKey,
    freshKeys.publicKey,
    unsigned.transactionId
  );
  const immutable = new Map<number, unknown>([
    [0, [pair(oldWitness)]],
    [1, [[0, Buffer.alloc(28, 1)]]],
    [
      2,
      [[oldKeys.publicKey, oldWitness.signature, Buffer.alloc(32), new Map()]],
    ],
    [3, [Buffer.from([1])]],
    [4, [0]],
    [5, [[0, 0, 0, [1, 2]]]],
    [6, [Buffer.from([2])]],
    [7, [Buffer.from([3])]],
  ]);
  const original = envelopeWith(immutable);
  const returned = encodeVKeyWitnessSet([oldWitness, freshWitness]);
  const delta = diffVKeyWitnesses(original, original.transactionId, returned);
  expect(extractVKeyWitnesses(delta).map((item) => item.publicKey)).toEqual([
    freshKeys.publicKey,
  ]);
  const required = extractVKeyWitnesses(delta)[0].keyHash.toString('hex');
  expect(() =>
    diffVKeyWitnesses(original, original.transactionId, returned, [required])
  ).not.toThrow();
  expect(() =>
    diffVKeyWitnesses(original, original.transactionId, returned, [
      'ff'.repeat(28),
    ])
  ).toThrow(WitnessSetError);
  expect(() => diffVKeyWitnesses(original, '00'.repeat(32), returned)).toThrow(
    WitnessSetError
  );

  const mergedBytes = mergeVKeyWitnesses(original, extractVKeyWitnesses(delta));
  const merged = parseConwayTransactionEnvelope(mergedBytes);
  expect(bytesForSpan(mergedBytes, merged.spans.body)).toEqual(
    bytesForSpan(original.cbor, original.spans.body)
  );
  expect(merged.transactionId).toBe(original.transactionId);
  const before = rawFields(original.cbor);
  const after = rawFields(mergedBytes);
  for (let field = 1; field <= 7; field += 1)
    expect(after.get(field)).toBe(before.get(field));
  expect(
    extractVKeyWitnesses(encodeVKeyWitnessSet([oldWitness, freshWitness]))
  ).toHaveLength(2);
  expect(() => mergeVKeyWitnesses(original, [oldWitness])).toThrow(
    WitnessSetError
  );
});

test('merges an empty witness delta without adding a VKey field', () => {
  const original = envelopeWith();
  expect(mergeVKeyWitnesses(original, [])).toEqual(original.cbor);
});

test('does not accept immutable witness classes as endpoint results', () => {
  const candidate = cbor.encodeCanonical(
    new Map([
      [0, []],
      [4, [0]],
    ])
  );
  expect(() => extractVKeyWitnesses(candidate)).toThrow(WitnessSetError);
  expect(() => parseCborItem(candidate)).not.toThrow();
});
