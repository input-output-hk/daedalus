// `crypto.getRandomValues` rejects requests larger than this. Chunking around
// the limit would hide it; refusing states it.
const MAX_BYTES = 65536;

// Continuous health test state. A CSPRNG that has failed open typically returns
// zeros or repeats itself, and at the sizes used here neither can happen by
// chance: an all-zero 32-byte draw has probability 2^-256.
let previousDraw: string | null = null;

/**
 * The entropy source for every wallet Daedalus creates.
 *
 * `generateMnemonic` passes this to bip39 explicitly instead of letting bip39
 * choose. bip39's default rng is a transitive dependency's default: 3.0.4 used
 * the `randombytes` package, 3.1.0 uses `@noble/hashes`. Both happen to be
 * sound, but neither is a decision this repository made, recorded, or asserted,
 * and a version bump replaces it silently.
 *
 * This function fails closed. With no platform CSPRNG it throws. There is no
 * fallback and there must never be one: a weaker source that works is worse
 * than an error that stops.
 */
export const secureRandomBytes = (size: number): Buffer => {
  if (!Number.isInteger(size) || size <= 0 || size > MAX_BYTES) {
    throw new Error(
      `secureRandomBytes: refusing to generate ${size} bytes; expected an integer between 1 and ${MAX_BYTES}`
    );
  }

  const source = globalThis.crypto;

  if (!source || typeof source.getRandomValues !== 'function') {
    throw new Error(
      'secureRandomBytes: no platform CSPRNG available; crypto.getRandomValues is required'
    );
  }

  // `new Uint8Array` is zero-filled by specification and has no unsafe variant,
  // unlike `Buffer.allocUnsafe`, which `randombytes` uses. That matters: with
  // an unsafe allocation, a source that silently did nothing would return
  // whatever occupied that heap page, which looks random and is not. Here the
  // same failure yields zeros and trips the check below.
  const bytes = new Uint8Array(size);
  source.getRandomValues(bytes);

  if (bytes.every((byte) => byte === 0)) {
    throw new Error(
      'secureRandomBytes: entropy source returned all zeros; refusing to use it'
    );
  }

  const buffer = Buffer.from(bytes);
  const draw = buffer.toString('hex');

  if (draw === previousDraw) {
    throw new Error(
      'secureRandomBytes: entropy source repeated its previous output; refusing to use it'
    );
  }

  previousDraw = draw;
  return buffer;
};
