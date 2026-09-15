import blake2b from 'blake2b';
import { bech32 } from 'bech32';

const HEX_BYTES = /^(?:[0-9a-fA-F]{2})*$/;
const FINGERPRINT_DIGEST_BYTES = 20;
const FINGERPRINT_PREFIX = 'asset';
// A minting policy id is the 28-byte hash of the policy's script, and an asset
// name is at most 32 bytes. Both are consensus limits rather than conventions.
const POLICY_ID_BYTES = 28;
const ASSET_NAME_MAX_BYTES = 32;

const decodeHex = (valueInHex: string, field: string): Uint8Array => {
  if (!HEX_BYTES.test(valueInHex)) {
    throw new Error(`assetFingerprint: ${field} is not a hex string`);
  }

  // `Buffer.from` yields a Buffer built against whichever realm supplies the
  // global, and `blake2b` guards its input with `instanceof Uint8Array`
  // resolved in its own. Under the jsdom test environment those are two
  // different realms and the guard rejects a perfectly good Buffer, so the
  // bytes are copied into a Uint8Array constructed here.
  const bytes = Buffer.from(valueInHex, 'hex');
  const copy = new Uint8Array(bytes.length);
  copy.set(bytes);
  return copy;
};

/**
 * Computes the CIP-14 fingerprint of an asset from its policy id and its asset
 * name, both hex-encoded: blake2b with a 20-byte digest over the concatenated
 * raw bytes, bech32-encoded with the human-readable part `asset`.
 *
 * This is the identifier a token row renders when no name resolves, and it is
 * derived rather than fetched, so it is available with a cold cache and needs
 * no round trip to draw a row.
 *
 * Both lengths are checked as well as both encodings. The two arguments are
 * both hex strings and swapping them produces a different, perfectly
 * well-formed fingerprint for a subject that does not exist, which the CIP-14
 * golden vectors demonstrate with a transposed pair. Each rule throws its own
 * message so a caller can tell a malformed value from a transposed one.
 */
export const assetFingerprint = (
  policyId: string,
  assetName: string
): string => {
  const policyIdBytes = decodeHex(policyId, 'policyId');

  if (policyIdBytes.length !== POLICY_ID_BYTES) {
    throw new Error(
      `assetFingerprint: policyId must be ${POLICY_ID_BYTES} bytes, was ${policyIdBytes.length}`
    );
  }

  const assetNameBytes = decodeHex(assetName, 'assetName');

  if (assetNameBytes.length > ASSET_NAME_MAX_BYTES) {
    throw new Error(
      `assetFingerprint: assetName must be at most ${ASSET_NAME_MAX_BYTES} bytes, was ${assetNameBytes.length}`
    );
  }

  const subject = new Uint8Array(policyIdBytes.length + assetNameBytes.length);
  subject.set(policyIdBytes);
  subject.set(assetNameBytes, policyIdBytes.length);
  const digest = blake2b(FINGERPRINT_DIGEST_BYTES).update(subject).digest();
  return bech32.encode(FINGERPRINT_PREFIX, bech32.toWords(digest));
};
