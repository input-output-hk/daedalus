import crypto from 'crypto';
import blake2b from 'blake2b';
import * as cbor from 'cbor';
import type {
  RegistryProperty,
  RegistrySignature,
} from './assetRegistryClient';

const POLICY_ID_HEX_LENGTH = 56;
const SIGNATURE_HEX_LENGTH = 128;
const PAYLOAD_DIGEST_BYTES = 32;

/**
 * The twelve-byte DER SubjectPublicKeyInfo header for an ed25519 key. Node's
 * crypto accepts a key object rather than raw bytes, and this is the whole of
 * the conversion.
 */
const ED25519_SPKI_PREFIX = Buffer.from('302a300506032b6570032100', 'hex');
const KEY_HASH_BYTES = 28;
const PUBLIC_KEY_HEX_LENGTH = 64;
const HEX_PATTERN = /^[0-9a-fA-F]*$/;

/**
 * The corpus maximum is 2 and the largest script holds 10 nodes, so this cannot
 * affect a real policy. It is here to make the recursion bounded by
 * construction rather than by argument.
 */
const MAX_SCRIPT_DEPTH = 32;

/**
 * Native script tags as the ledger encodes them. Anything else is not a native
 * script and is refused rather than interpreted.
 */
const TAG_SIG = 0;
const TAG_ALL = 1;
const TAG_ANY = 2;
const TAG_AT_LEAST = 3;
const TAG_TIME_AFTER = 4;
const TAG_TIME_BEFORE = 5;

export type NativeScript =
  | { kind: 'sig'; keyHash: string }
  | { kind: 'all'; scripts: Array<NativeScript> }
  | { kind: 'any'; scripts: Array<NativeScript> }
  | { kind: 'atLeast'; required: number; scripts: Array<NativeScript> }
  | { kind: 'timeAfter'; slot: number }
  | { kind: 'timeBefore'; slot: number };

export type PolicyBindingFailure =
  | 'absent'
  | 'malformed'
  | 'digest-mismatch'
  | 'not-a-script';

export type PolicyVerificationResult =
  | {
      bound: true;
      satisfied: boolean;
      script: NativeScript;
      policyId: string;
    }
  | {
      bound: false;
      reason: PolicyBindingFailure;
      expectedPolicyId?: string;
      actualPolicyId?: string;
    };

const isHex = (value: string): boolean =>
  value.length % 2 === 0 && HEX_PATTERN.test(value);

// blake2b guards its input with a realm-sensitive `instanceof Uint8Array`, so
// the array is built here rather than handed over as a Buffer.
const digestBytes = (size: number, bytes: Uint8Array): Uint8Array => {
  const input = new Uint8Array(bytes.length);
  input.set(bytes);
  return blake2b(size).update(input).digest();
};

const digest = (size: number, bytes: Uint8Array): string =>
  Buffer.from(digestBytes(size, bytes)).toString('hex');

export const assetKeyHash = (publicKey: string): string | null => {
  if (
    typeof publicKey !== 'string' ||
    publicKey.length !== PUBLIC_KEY_HEX_LENGTH ||
    !isHex(publicKey)
  ) {
    return null;
  }
  return digest(KEY_HASH_BYTES, Buffer.from(publicKey, 'hex'));
};

export const attestingKeyHashes = (
  signatures: Array<RegistrySignature>
): Set<string> => {
  const hashes = new Set<string>();
  signatures.forEach((signature) => {
    const hash = assetKeyHash(signature?.publicKey);
    if (hash) hashes.add(hash);
  });
  return hashes;
};

const isInteger = (value: unknown): value is number =>
  typeof value === 'number' && Number.isInteger(value);

export const decodeNativeScript = (
  value: unknown,
  depth = 0
): NativeScript | null => {
  if (depth > MAX_SCRIPT_DEPTH) return null;
  if (!Array.isArray(value) || value.length === 0) return null;
  const [tag] = value;
  switch (tag) {
    case TAG_SIG: {
      const keyHash = value[1];
      if (!(keyHash instanceof Uint8Array)) return null;
      if (keyHash.length !== KEY_HASH_BYTES) return null;
      return {
        kind: 'sig',
        keyHash: Buffer.from(keyHash).toString('hex'),
      };
    }
    case TAG_ALL:
    case TAG_ANY: {
      if (!Array.isArray(value[1])) return null;
      const scripts = decodeNativeScripts(value[1], depth);
      if (!scripts) return null;
      return { kind: tag === TAG_ALL ? 'all' : 'any', scripts };
    }
    case TAG_AT_LEAST: {
      const required = value[1];
      if (!isInteger(required) || required < 0) return null;
      if (!Array.isArray(value[2])) return null;
      const scripts = decodeNativeScripts(value[2], depth);
      if (!scripts) return null;
      return { kind: 'atLeast', required, scripts };
    }
    case TAG_TIME_AFTER:
    case TAG_TIME_BEFORE: {
      const slot = value[1];
      if (!isInteger(slot)) return null;
      return {
        kind: tag === TAG_TIME_AFTER ? 'timeAfter' : 'timeBefore',
        slot,
      };
    }
    default:
      return null;
  }
};

function decodeNativeScripts(
  values: Array<unknown>,
  depth: number
): Array<NativeScript> | null {
  const scripts: Array<NativeScript> = [];
  for (let index = 0; index < values.length; index += 1) {
    const script = decodeNativeScript(values[index], depth + 1);
    if (!script) return null;
    scripts.push(script);
  }
  return scripts;
}

/**
 * `evaluatePolicy` from the registry's own implementation, transcribed. Both
 * time-lock forms are satisfied regardless of the current slot, which is what
 * the reference does and why this function takes no clock. Checking an upper
 * bound against the clock would fail every already-expired policy, which is
 * most of the corpus.
 *
 * A key-hash lookup is deliberately not used in place of this. It is undefined
 * for `any` and `atLeast`, where no single key is required and one signer does
 * not satisfy the policy.
 */
export const evaluateNativeScript = (
  script: NativeScript,
  keyHashes: Set<string>
): boolean => {
  switch (script.kind) {
    case 'sig':
      return keyHashes.has(script.keyHash);
    case 'all':
      return script.scripts.every((inner) =>
        evaluateNativeScript(inner, keyHashes)
      );
    case 'any':
      return script.scripts.some((inner) =>
        evaluateNativeScript(inner, keyHashes)
      );
    case 'atLeast':
      return (
        script.scripts.filter((inner) => evaluateNativeScript(inner, keyHashes))
          .length >= script.required
      );
    case 'timeAfter':
    case 'timeBefore':
      return true;
    default:
      return false;
  }
};

export const nativeScriptPolicyId = (scriptBytes: Uint8Array): string => {
  const prefixed = new Uint8Array(scriptBytes.length + 1);
  prefixed[0] = 0;
  prefixed.set(scriptBytes, 1);
  return digest(KEY_HASH_BYTES, prefixed);
};

/**
 * Steps one and two of the three the PRD specifies. The verdict this returns is
 * not `verified`: the attestation signature is step three, and `task-010` is
 * where the three are combined into the column.
 *
 * The digest is taken before the script is decoded, and that ordering is the
 * point. The decoder only ever runs on bytes that already hash to the subject's
 * own policy id, which a server controlling the response cannot produce for a
 * policy it does not control.
 */
export const verifyPolicyBinding = (
  subject: string,
  policy: string | null | undefined,
  signatures: Array<RegistrySignature>
): PolicyVerificationResult => {
  if (typeof policy !== 'string' || policy.length === 0) {
    // The ordinary state of a large share of the registry, not an error.
    return { bound: false, reason: 'absent' };
  }
  if (
    typeof subject !== 'string' ||
    subject.length < POLICY_ID_HEX_LENGTH ||
    !isHex(subject.slice(0, POLICY_ID_HEX_LENGTH))
  ) {
    return { bound: false, reason: 'malformed' };
  }
  // Two bytes of array header and era tag. Measured over 405 policy-bearing
  // mainnet subjects: the strip reproduces the policy id for all 405 and
  // hashing the whole field reproduces it for none.
  if (!isHex(policy) || policy.length <= 4) {
    return { bound: false, reason: 'malformed' };
  }

  const scriptBytes = Buffer.from(policy.slice(4), 'hex');
  const expectedPolicyId = subject.slice(0, POLICY_ID_HEX_LENGTH).toLowerCase();
  const actualPolicyId = nativeScriptPolicyId(scriptBytes);
  if (actualPolicyId !== expectedPolicyId) {
    return {
      bound: false,
      reason: 'digest-mismatch',
      expectedPolicyId,
      actualPolicyId,
    };
  }

  let decoded: unknown;
  try {
    decoded = cbor.decodeFirstSync(scriptBytes);
  } catch {
    return { bound: false, reason: 'not-a-script' };
  }
  const script = decodeNativeScript(decoded);
  if (!script) return { bound: false, reason: 'not-a-script' };

  return {
    bound: true,
    satisfied: evaluateNativeScript(script, attestingKeyHashes(signatures)),
    script,
    policyId: actualPolicyId,
  };
};

/**
 * The registry's attestation payload:
 *
 *   blake2b256( blake2b256(CBOR(subject))
 *            || blake2b256(CBOR(propertyName))
 *            || blake2b256(CBOR(value))
 *            || blake2b256(CBOR(sequenceNumber)) )
 *
 * The subject and the property name are CBOR text strings before they are
 * hashed. Hashing them as raw UTF-8 reproduces no signature in the registry.
 *
 * `logo` is the one property whose value is not encoded as it arrives: it is
 * base64 text on the wire and is signed as a CBOR byte string over the decoded
 * bytes. Confirmed against a live 77,392-character logo, where the base64-text
 * form does not verify and the decoded-bytes form does.
 */
export const attestationPayload = (
  subject: string,
  propertyName: string,
  value: unknown,
  sequenceNumber: number
): Buffer | null => {
  if (typeof subject !== 'string' || typeof propertyName !== 'string') {
    return null;
  }
  // A float encodes to different CBOR bytes than an integer, so a sequence
  // number that is not one would produce a payload that silently never
  // verifies.
  if (!Number.isInteger(sequenceNumber)) return null;
  let encodedValue: Buffer;
  try {
    if (propertyName === 'logo') {
      if (typeof value !== 'string') return null;
      encodedValue = cbor.encode(Buffer.from(value, 'base64'));
    } else {
      encodedValue = cbor.encode(value);
    }
    return Buffer.from(
      digestBytes(
        PAYLOAD_DIGEST_BYTES,
        Buffer.concat([
          digestBytes(PAYLOAD_DIGEST_BYTES, cbor.encode(subject)),
          digestBytes(PAYLOAD_DIGEST_BYTES, cbor.encode(propertyName)),
          digestBytes(PAYLOAD_DIGEST_BYTES, encodedValue),
          digestBytes(PAYLOAD_DIGEST_BYTES, cbor.encode(sequenceNumber)),
        ])
      )
    );
  } catch {
    return null;
  }
};

/**
 * Node's built-in verifier, which is strict: it rejects a signature whose
 * scalar S has had the group order added to it. `cardano-crypto.js` accepts
 * one, so it is not used for this, and Node's adds nothing to the dependency
 * tree.
 */
export const verifyAttestationSignature = (
  payload: Buffer,
  signature: string,
  publicKey: string
): boolean => {
  if (
    typeof signature !== 'string' ||
    signature.length !== SIGNATURE_HEX_LENGTH ||
    !isHex(signature)
  ) {
    return false;
  }
  if (
    typeof publicKey !== 'string' ||
    publicKey.length !== PUBLIC_KEY_HEX_LENGTH ||
    !isHex(publicKey)
  ) {
    return false;
  }
  try {
    const key = crypto.createPublicKey({
      key: Buffer.concat([ED25519_SPKI_PREFIX, Buffer.from(publicKey, 'hex')]),
      format: 'der',
      type: 'spki',
    });
    return crypto.verify(null, payload, key, Buffer.from(signature, 'hex'));
  } catch {
    return false;
  }
};

/**
 * A property is attested when any one of its signatures verifies. 104
 * properties in the registry carry more than one, and an attestation is a set
 * rather than a single signature.
 */
export const isPropertyAttested = (
  subject: string,
  propertyName: string,
  property: RegistryProperty
): boolean => {
  if (!property || !Array.isArray(property.signatures)) return false;
  const payload = attestationPayload(
    subject,
    propertyName,
    property.value,
    property.sequenceNumber
  );
  if (!payload) return false;
  return property.signatures.some((signature) =>
    verifyAttestationSignature(
      payload,
      signature?.signature,
      signature?.publicKey
    )
  );
};

export type PropertyVerificationResult = {
  bound: boolean;
  satisfied: boolean;
  attested: boolean;
  verified: boolean;
};

/**
 * All three steps for one property. Each field reports its own fact and none is
 * short-circuited, because `attested: false` because a signature is wrong and
 * `attested: false` because nobody looked are different sentences and the
 * advisory shown to a user has to tell them apart.
 *
 * `verified` is the conjunction and is the only field to test for a verdict. It
 * is computed here from the bytes and is never read from a field of a registry
 * response.
 */
export const verifyRegistryProperty = (
  subject: string,
  policy: string | null | undefined,
  propertyName: string,
  property: RegistryProperty
): PropertyVerificationResult => {
  const binding = verifyPolicyBinding(
    subject,
    policy,
    property?.signatures ?? []
  );
  const bound = binding.bound === true;
  const satisfied = binding.bound === true && binding.satisfied;
  const attested = isPropertyAttested(subject, propertyName, property);
  return {
    bound,
    satisfied,
    attested,
    verified: bound && satisfied && attested,
  };
};
