/**
 * @jest-environment node
 *
 * The crypto path touches no DOM. It runs in the Electron renderer, which is
 * Chromium, and in the main process, which is Node; both are single-realm.
 * `jest-environment-jsdom` is neither: it injects Node's `Buffer` into a
 * context whose `Uint8Array` is its own, so `Buffer.from(x) instanceof
 * Uint8Array` is false there and true in every environment that ships.
 *
 * That distinction is not hypothetical here. `blakejs` 1.2.1 dropped the
 * `input instanceof Buffer` branch that 1.1.0 carried and now relies solely on
 * `instanceof Uint8Array`, so under jsdom it rejects a Buffer that both real
 * environments accept.
 */
import { secureRandomBytes } from './entropy';

const realCrypto = globalThis.crypto;

const setSource = (value: unknown) =>
  Object.defineProperty(globalThis, 'crypto', {
    value,
    configurable: true,
    writable: true,
  });

const fillWith = (pattern: number[]) => (view: Uint8Array) => {
  for (let i = 0; i < view.length; i += 1) {
    view[i] = pattern[i % pattern.length];
  }
  return view;
};

afterEach(() => setSource(realCrypto));

describe('secureRandomBytes', () => {
  it('returns the number of bytes it was asked for', () => {
    expect(secureRandomBytes(32)).toHaveLength(32);
    expect(secureRandomBytes(20)).toHaveLength(20);
    expect(secureRandomBytes(1)).toHaveLength(1);
  });

  it('returns exactly the bytes the platform source produced', () => {
    const pattern = [0x11, 0x22, 0x33, 0x44];
    setSource({ getRandomValues: fillWith(pattern) });
    expect(secureRandomBytes(8).toString('hex')).toBe('1122334411223344');
  });

  it('asks the platform source for a view of the requested length', () => {
    const getRandomValues = jest.fn(fillWith([0xab, 0xcd]));
    setSource({ getRandomValues });
    secureRandomBytes(32);
    expect(getRandomValues).toHaveBeenCalledTimes(1);
    expect(getRandomValues.mock.calls[0][0]).toHaveLength(32);
  });

  it('returns a Buffer, as the bip39 rng contract expects', () => {
    expect(Buffer.isBuffer(secureRandomBytes(32))).toBe(true);
  });

  it('does not repeat itself across calls', () => {
    expect(secureRandomBytes(32).toString('hex')).not.toBe(
      secureRandomBytes(32).toString('hex')
    );
  });
});

describe('secureRandomBytes failure behaviour', () => {
  // Each case below must throw rather than return something weaker. A fallback
  // that produces bytes anyway is the failure this module exists to prevent.

  it('throws when the platform has no crypto object', () => {
    setSource(undefined);
    expect(() => secureRandomBytes(32)).toThrow('no platform CSPRNG available');
  });

  it('throws when getRandomValues is missing', () => {
    setSource({});
    expect(() => secureRandomBytes(32)).toThrow('no platform CSPRNG available');
  });

  it('throws when getRandomValues is not callable', () => {
    setSource({ getRandomValues: 'not a function' });
    expect(() => secureRandomBytes(32)).toThrow('no platform CSPRNG available');
  });

  it('throws when the source leaves the buffer all zeros', () => {
    // `Buffer.alloc` zero-fills, so a source that silently does nothing is
    // caught here. `allocUnsafe` would have returned live heap contents, which
    // look random and are not.
    setSource({ getRandomValues: (view: Uint8Array) => view });
    expect(() => secureRandomBytes(32)).toThrow('returned all zeros');
  });

  it('throws when the source returns explicit zeros', () => {
    setSource({ getRandomValues: fillWith([0x00]) });
    expect(() => secureRandomBytes(32)).toThrow('returned all zeros');
  });

  it('throws when the source repeats its previous output', () => {
    setSource({ getRandomValues: fillWith([0x5a, 0xa5, 0x3c, 0xc3]) });
    expect(secureRandomBytes(16)).toHaveLength(16);
    expect(() => secureRandomBytes(16)).toThrow('repeated its previous output');
  });

  it.each([
    [0, 'zero'],
    [-1, 'negative'],
    [1.5, 'fractional'],
    [NaN, 'not a number'],
    [Infinity, 'infinite'],
    [65537, 'above the getRandomValues limit'],
  ])('refuses a %p size, being %s', (size) => {
    expect(() => secureRandomBytes(size)).toThrow('refusing to generate');
  });
});
