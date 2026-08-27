import { createPublicKey, verify } from 'crypto';
import { blake2b } from 'blakejs';
import cbor from 'cbor';

import { bytesForSpan, CborItem, parseCborItem } from './cborSlices';
import { ExactTransactionEnvelope } from './transactionEnvelope';

export class WitnessSetError extends Error {
  constructor() {
    super('Invalid witness set');
    this.name = 'WitnessSetError';
  }
}

export type VKeyWitnessData = Readonly<{
  publicKey: Buffer;
  signature: Buffer;
}>;

export type VKeyWitness = VKeyWitnessData &
  Readonly<{
    keyHash: Buffer;
    cbor: Buffer;
  }>;

const invalid = (): never => {
  throw new WitnessSetError();
};
const entries = (
  item: CborItem
): readonly Readonly<{ key: CborItem; value: CborItem }>[] =>
  item.major === 5 && item.entries ? item.entries : invalid();
const array = (item: CborItem): readonly CborItem[] =>
  item.major === 4 && item.items ? item.items : invalid();
const definiteBytes = (
  source: Buffer,
  item: CborItem,
  length: number
): Buffer => {
  if (item.major !== 2 || !item.content) invalid();
  const value = bytesForSpan(source, item.content);
  if (value.length !== length) invalid();
  return value;
};

const setMembers = (item: CborItem): readonly CborItem[] => {
  let content = item;
  if (item.major === 6) {
    if (item.tag !== BigInt(258) || !item.items || item.items.length !== 1)
      invalid();
    [content] = item.items;
  }
  return array(content);
};

const parseVKey = (source: Buffer, item: CborItem): VKeyWitness => {
  const parts = array(item);
  if (parts.length !== 2) invalid();
  const publicKey = definiteBytes(source, parts[0], 32);
  const signature = definiteBytes(source, parts[1], 64);
  return {
    publicKey: Buffer.from(publicKey),
    signature: Buffer.from(signature),
    keyHash: Buffer.from(blake2b(publicKey, undefined, 28)),
    cbor: Buffer.from(bytesForSpan(source, item.span)),
  };
};

const decodeWitnessSet = (
  source: Buffer,
  vkeyOnly: boolean
): readonly VKeyWitness[] => {
  try {
    const root = parseCborItem(source);
    if (root.span.end !== source.length) invalid();
    let vkeys: CborItem | undefined;
    for (const { key, value } of entries(root)) {
      if (key.major !== 0 || key.value === undefined) invalid();
      const field = Number(key.value);
      if (field < 0 || field > 7 || (vkeyOnly && field !== 0)) invalid();
      if (field === 0) vkeys = value;
    }
    if (!vkeys) return [];
    const members = setMembers(vkeys);
    if (members.length === 0) invalid();
    const witnesses = members.map((member) => parseVKey(source, member));
    const keys = new Set(
      witnesses.map(({ publicKey }) => publicKey.toString('hex'))
    );
    if (keys.size !== witnesses.length) invalid();
    return witnesses;
  } catch (error) {
    if (error instanceof WitnessSetError) throw error;
    throw new WitnessSetError();
  }
};

export const extractVKeyWitnesses = (
  witnessSetCbor: Buffer
): readonly VKeyWitness[] => decodeWitnessSet(witnessSetCbor, true);

export const verifyVKeyWitness = (
  bodyHash: Buffer,
  witness: VKeyWitnessData
): void => {
  try {
    if (
      bodyHash.length !== 32 ||
      witness.publicKey.length !== 32 ||
      witness.signature.length !== 64
    )
      invalid();
    const publicKey = createPublicKey({
      key: Buffer.concat([
        Buffer.from('302a300506032b6570032100', 'hex'),
        witness.publicKey,
      ]),
      format: 'der',
      type: 'spki',
    });
    if (!verify(null, bodyHash, publicKey, witness.signature)) invalid();
  } catch (error) {
    if (error instanceof WitnessSetError) throw error;
    throw new WitnessSetError();
  }
};

export const verifyVKeyWitnesses = (
  bodyBytes: Buffer,
  witnesses: readonly VKeyWitnessData[]
): void => {
  const bodyHash = Buffer.from(blake2b(bodyBytes, undefined, 32));
  witnesses.forEach((witness) => verifyVKeyWitness(bodyHash, witness));
};

export const encodeVKeyWitnessSet = (
  witnesses: readonly VKeyWitnessData[]
): Buffer => {
  const keys = new Set<string>();
  const encoded = witnesses.map(({ publicKey, signature }) => {
    if (publicKey.length !== 32 || signature.length !== 64) invalid();
    const key = publicKey.toString('hex');
    if (keys.has(key)) invalid();
    keys.add(key);
    return [publicKey, signature];
  });
  return cbor.encodeCanonical(
    encoded.length === 0 ? new Map() : new Map([[0, encoded]])
  );
};

const originalVKeys = (envelope: ExactTransactionEnvelope) =>
  decodeWitnessSet(
    bytesForSpan(envelope.cbor, envelope.spans.witnessSet),
    false
  );

export const diffVKeyWitnesses = (
  envelope: ExactTransactionEnvelope,
  returnedBodyHash: string,
  returnedWitnessSetCbor: Buffer,
  requiredKeyHashes: readonly string[] = []
): Buffer => {
  if (returnedBodyHash !== envelope.transactionId) invalid();
  const bodyBytes = bytesForSpan(envelope.cbor, envelope.spans.body);
  const original = originalVKeys(envelope);
  const returned = extractVKeyWitnesses(returnedWitnessSetCbor);
  verifyVKeyWitnesses(bodyBytes, original);
  verifyVKeyWitnesses(bodyBytes, returned);
  const allHashes = new Set(
    [...original, ...returned].map(({ keyHash }) => keyHash.toString('hex'))
  );
  if (
    requiredKeyHashes.some(
      (keyHash) => !/^[0-9a-f]{56}$/u.test(keyHash) || !allHashes.has(keyHash)
    )
  )
    invalid();
  const existing = new Set(
    original.map(({ publicKey }) => publicKey.toString('hex'))
  );
  return encodeVKeyWitnessSet(
    returned.filter(({ publicKey }) => !existing.has(publicKey.toString('hex')))
  );
};

const canonicalVKeyPair = (witnesses: readonly VKeyWitnessData[]): Buffer => {
  const encoded = encodeVKeyWitnessSet(witnesses);
  const [entry] = entries(parseCborItem(encoded));
  if (!entry) invalid();
  return Buffer.from(
    encoded.subarray(entry.key.span.start, entry.value.span.end)
  );
};

export const mergeVKeyWitnesses = (
  envelope: ExactTransactionEnvelope,
  freshWitnesses: readonly VKeyWitnessData[]
): Buffer => {
  const bodyBytes = bytesForSpan(envelope.cbor, envelope.spans.body);
  const original = originalVKeys(envelope);
  verifyVKeyWitnesses(bodyBytes, original);
  verifyVKeyWitnesses(bodyBytes, freshWitnesses);
  const keys = new Set(
    original.map(({ publicKey }) => publicKey.toString('hex'))
  );
  freshWitnesses.forEach(({ publicKey }) => {
    const key = publicKey.toString('hex');
    if (keys.has(key)) invalid();
    keys.add(key);
  });

  const source = envelope.cbor;
  const originalEntries = entries(envelope.witnessSet);
  const combined = [...original, ...freshWitnesses];
  const pair = combined.length === 0 ? undefined : canonicalVKeyPair(combined);
  const fields: Buffer[] = [];
  let replaced = false;
  originalEntries.forEach((entry) => {
    if (entry.key.major === 0 && entry.key.value === BigInt(0)) {
      if (!pair) invalid();
      fields.push(pair);
      replaced = true;
    } else {
      fields.push(
        Buffer.from(source.subarray(entry.key.span.start, entry.value.span.end))
      );
    }
  });
  if (!replaced && pair) fields.unshift(pair);
  if (fields.length > 23) invalid();
  const witnessSet = Buffer.concat([
    Buffer.from([0xa0 + fields.length]),
    ...fields,
  ]);
  return Buffer.concat([
    source.subarray(0, envelope.spans.witnessSet.start),
    witnessSet,
    source.subarray(envelope.spans.witnessSet.end),
  ]);
};
