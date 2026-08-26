import cbor from 'cbor';

import { bytesForSpan, CborItem, parseCborItem } from './cborSlices';

export class CoseError extends Error {
  public constructor() {
    super('Invalid CIP-8 COSE');
    this.name = 'CoseError';
  }
}

const invalid = (): never => {
  throw new CoseError();
};

const completeItem = (source: Buffer): CborItem => {
  const item = parseCborItem(source);
  if (item.span.end !== source.length) invalid();
  return item;
};

const definiteBytes = (
  source: Buffer,
  item: CborItem,
  length?: number
): Buffer => {
  if (item.major !== 2 || !item.content) return invalid();
  const value = bytesForSpan(source, item.content);
  if (length !== undefined && value.length !== length) return invalid();
  return value;
};

export const encodeCoseProtectedHeader = (address: Buffer): Buffer =>
  cbor.encodeCanonical(
    new Map<unknown, unknown>([
      [1, -8],
      ['address', address],
    ])
  );

export const encodeCoseSignatureStructure = (
  protectedHeader: Buffer,
  payload: Buffer
): Buffer =>
  cbor.encodeCanonical([
    'Signature1',
    protectedHeader,
    Buffer.alloc(0),
    payload,
  ]);

const encodeSign1 = (
  address: Buffer,
  payload: Buffer,
  signature: Buffer,
  includeVersion: boolean
): Buffer => {
  if (!address.length || signature.length !== 64) return invalid();
  const unprotected = includeVersion
    ? { hashed: false, version: 1 }
    : { hashed: false };
  return cbor.encodeCanonical([
    encodeCoseProtectedHeader(address),
    unprotected,
    payload,
    signature,
  ]);
};

export const encodeCoseSign1 = (
  address: Buffer,
  payload: Buffer,
  signature: Buffer
): Buffer => encodeSign1(address, payload, signature, true);

export type DecodedCoseSign1 = Readonly<{
  protectedHeader: Buffer;
  signature: Buffer;
}>;

export const decodeCoseSign1 = (
  source: Buffer,
  expectedAddress: Buffer,
  expectedPayload: Buffer,
  allowLegacyMissingVersion = false
): DecodedCoseSign1 => {
  try {
    const root = completeItem(source);
    if (root.major !== 4 || !root.items || root.items.length !== 4)
      return invalid();
    const protectedHeader = definiteBytes(source, root.items[0]);
    const signature = definiteBytes(source, root.items[3], 64);
    const expected = encodeSign1(
      expectedAddress,
      expectedPayload,
      signature,
      true
    );
    if (
      !source.equals(expected) &&
      (!allowLegacyMissingVersion ||
        !source.equals(
          encodeSign1(expectedAddress, expectedPayload, signature, false)
        ))
    )
      return invalid();
    return {
      protectedHeader: Buffer.from(protectedHeader),
      signature: Buffer.from(signature),
    };
  } catch (error) {
    if (error instanceof CoseError) throw error;
    throw new CoseError();
  }
};

export const encodeCoseKey = (publicKey: Buffer): Buffer => {
  if (publicKey.length !== 32) return invalid();
  return cbor.encodeCanonical(
    new Map<number, number | Buffer>([
      [1, 1],
      [3, -8],
      [-1, 6],
      [-2, publicKey],
    ])
  );
};

export const decodeCoseKey = (source: Buffer): Buffer => {
  try {
    const root = completeItem(source);
    if (root.major !== 5 || !root.entries) return invalid();
    const publicKeyEntry = root.entries.find(
      ({ key }) => key.major === 1 && key.value === BigInt(-2)
    );
    if (!publicKeyEntry) return invalid();
    const publicKey = definiteBytes(source, publicKeyEntry.value, 32);
    if (!source.equals(encodeCoseKey(publicKey))) return invalid();
    return Buffer.from(publicKey);
  } catch (error) {
    if (error instanceof CoseError) throw error;
    throw new CoseError();
  }
};
