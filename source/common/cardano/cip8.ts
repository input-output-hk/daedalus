import { createPublicKey, verify } from 'crypto';

import { blake2b } from 'blakejs';

import type { DataSignature } from '../cip30/wire';
import {
  Cip8Error,
  cip8HexBytes as hexBytes,
  decodeCip8Hex as decodeHex,
  invalidCip8 as invalid,
} from './cip8Request';
import type { Cip8CredentialKind, Cip8ExpectedRequest } from './cip8Request';
import {
  decodeCoseKey,
  decodeCoseSign1,
  encodeCoseKey,
  encodeCoseSign1,
  encodeCoseSignatureStructure,
} from './cose';

export type Cip8BackendResponse = Readonly<{
  revision: 1;
  credential_kind: Cip8CredentialKind;
  credential: string;
  cose_sign1: string;
  cose_key: string;
}>;

export type Cip8DataSignReview = Readonly<{
  address: string;
  credentialKind: Cip8CredentialKind;
  payload: string;
  utf8Preview: string | null;
}>;

const unsafePreview = /[\p{Cc}\p{Cf}\p{Cs}\p{Co}\p{Cn}\p{Zl}\p{Zp}]/u;
const safeUtf8Preview = (payload: Buffer): string | null => {
  const decoded = payload.toString('utf8');
  return Buffer.from(decoded, 'utf8').equals(payload) &&
    !unsafePreview.test(decoded)
    ? decoded
    : null;
};

export const createCip8DataSignReview = (
  expected: Cip8ExpectedRequest
): Cip8DataSignReview => {
  const utf8Preview = safeUtf8Preview(expected.payload);
  return Object.freeze({
    address: expected.address,
    credentialKind: expected.credentialKind,
    payload: expected.payload.toString('hex'),
    utf8Preview,
  });
};

export const parseCip8DataSignReview = (value: unknown): Cip8DataSignReview => {
  if (!value || typeof value !== 'object' || Array.isArray(value))
    return invalid();
  const review = value as Record<string, unknown>;
  if (
    Object.keys(review).sort().join(',') !==
      'address,credentialKind,payload,utf8Preview' ||
    typeof review.address !== 'string' ||
    review.address.length === 0 ||
    !hexBytes.test(review.address) ||
    (review.credentialKind !== 'payment' &&
      review.credentialKind !== 'stake' &&
      review.credentialKind !== 'drep') ||
    typeof review.payload !== 'string' ||
    !hexBytes.test(review.payload) ||
    (review.utf8Preview !== null && typeof review.utf8Preview !== 'string') ||
    review.utf8Preview !==
      safeUtf8Preview(Buffer.from(review.payload as string, 'hex'))
  )
    return invalid();
  return Object.freeze(review as Cip8DataSignReview);
};

const verifyEd25519 = (
  publicKey: Buffer,
  message: Buffer,
  signature: Buffer
): void => {
  try {
    const key = createPublicKey({
      key: Buffer.concat([
        Buffer.from('302a300506032b6570032100', 'hex'),
        publicKey,
      ]),
      format: 'der',
      type: 'spki',
    });
    if (!verify(null, message, key, signature)) invalid();
  } catch (error) {
    if (error instanceof Cip8Error) throw error;
    throw new Cip8Error();
  }
};

export type VerifyCip8Options = Readonly<{
  /** Verification-only compatibility for legacy wallets; producers always emit version:1. */
  allowLegacyMissingVersion?: boolean;
}>;

export const verifyCip8BackendResponse = (
  expected: Cip8ExpectedRequest,
  response: Cip8BackendResponse,
  { allowLegacyMissingVersion = false }: VerifyCip8Options = {}
): DataSignature => {
  try {
    if (
      response.revision !== 1 ||
      response.credential_kind !== expected.credentialKind ||
      response.credential !== expected.credential.toString('hex')
    )
      return invalid();

    const sign1Bytes = decodeHex(response.cose_sign1);
    const keyBytes = decodeHex(response.cose_key);
    const sign1 = decodeCoseSign1(
      sign1Bytes,
      expected.protectedAddress,
      expected.payload,
      allowLegacyMissingVersion
    );
    const publicKey = decodeCoseKey(keyBytes);
    const credential = Buffer.from(blake2b(publicKey, undefined, 28));
    if (!credential.equals(expected.credential)) return invalid();

    verifyEd25519(
      publicKey,
      encodeCoseSignatureStructure(sign1.protectedHeader, expected.payload),
      sign1.signature
    );
    return { signature: response.cose_sign1, key: response.cose_key };
  } catch (error) {
    if (error instanceof Cip8Error) throw error;
    throw new Cip8Error();
  }
};

export const serializeCip8 = (
  expected: Cip8ExpectedRequest,
  material: Readonly<{ publicKey: Buffer; signature: Buffer }>
): DataSignature => {
  const response: Cip8BackendResponse = {
    revision: 1,
    credential_kind: expected.credentialKind,
    credential: expected.credential.toString('hex'),
    cose_sign1: encodeCoseSign1(
      expected.protectedAddress,
      expected.payload,
      material.signature
    ).toString('hex'),
    cose_key: encodeCoseKey(material.publicKey).toString('hex'),
  };
  return verifyCip8BackendResponse(expected, response);
};
