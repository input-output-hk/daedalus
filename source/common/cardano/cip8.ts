import { createPublicKey, verify } from 'crypto';

import { blake2b } from 'blakejs';

import type { DataSignature } from '../cip30/wire';
import { normalizeCip30Address } from './cip30Serialization';
import {
  decodeCoseKey,
  decodeCoseSign1,
  encodeCoseKey,
  encodeCoseSign1,
  encodeCoseSignatureStructure,
} from './cose';

const MAX_PAYLOAD_BYTES = 65_536;
const hexBytes = /^(?:[0-9a-f]{2})*$/;
const hash28 = /^[0-9a-f]{56}$/;

export class Cip8Error extends Error {
  public constructor() {
    super('Invalid CIP-8 data signature');
    this.name = 'Cip8Error';
  }
}

const invalid = (): never => {
  throw new Cip8Error();
};

const decodeHex = (value: unknown, allowEmpty = false): Buffer => {
  if (
    typeof value !== 'string' ||
    !hexBytes.test(value) ||
    (!allowEmpty && value.length === 0)
  )
    return invalid();
  return Buffer.from(value, 'hex');
};

export type Cip8CredentialKind = 'payment' | 'stake' | 'drep';

export type Cip8ExpectedRequest = Readonly<{
  address: string;
  credentialKind: Cip8CredentialKind;
  credential: Buffer;
  protectedAddress: Buffer;
  payload: Buffer;
}>;

export type Cip8BackendResponse = Readonly<{
  revision: 1;
  credential_kind: Cip8CredentialKind;
  credential: string;
  cose_sign1: string;
  cose_key: string;
}>;

export type PrepareCip8RequestOptions = Readonly<{
  networkId: 0 | 1;
  drepCredential?: string;
}>;

export const prepareCip8Request = (
  address: string,
  payload: string,
  { networkId, drepCredential }: PrepareCip8RequestOptions
): Cip8ExpectedRequest => {
  try {
    const payloadBytes = decodeHex(payload, true);
    if (payloadBytes.length > MAX_PAYLOAD_BYTES) return invalid();
    if (drepCredential !== undefined && !hash28.test(drepCredential))
      return invalid();

    if (hash28.test(address)) {
      if (drepCredential === undefined) return invalid();
      const credential = Buffer.from(address, 'hex');
      return {
        address,
        credentialKind: 'drep',
        credential,
        protectedAddress: Buffer.from(credential),
        payload: payloadBytes,
      };
    }

    const normalized = normalizeCip30Address(address, networkId);
    const addressBytes = Buffer.from(normalized, 'hex');
    const type = addressBytes[0] >> 4;
    const credential = addressBytes.subarray(1, 29);
    if (credential.length !== 28) return invalid();

    let credentialKind: Cip8CredentialKind;
    let protectedAddress = addressBytes;
    if (type === 14) {
      credentialKind = 'stake';
    } else if (type === 0 || type === 2 || type === 4 || type === 6) {
      credentialKind = 'payment';
      if (
        type === 6 &&
        drepCredential !== undefined &&
        credential.toString('hex') === drepCredential
      ) {
        credentialKind = 'drep';
        protectedAddress = credential;
      }
    } else {
      return invalid();
    }

    return {
      address: normalized,
      credentialKind,
      credential: Buffer.from(credential),
      protectedAddress: Buffer.from(protectedAddress),
      payload: payloadBytes,
    };
  } catch (error) {
    if (error instanceof Cip8Error) throw error;
    throw new Cip8Error();
  }
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
