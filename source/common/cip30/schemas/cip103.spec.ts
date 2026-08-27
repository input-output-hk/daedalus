import { parseCip103SignRequest, parseCip103SubmitRequest } from './cip103';

const invalidRequest = { code: -1, info: 'Invalid request' };
const expectInvalid = (callback: () => unknown): void => {
  let caught: unknown;
  try {
    callback();
  } catch (error) {
    caught = error;
  }
  expect(caught).toEqual(invalidRequest);
};

describe('CIP-103 request schemas', () => {
  it('copies requests and normalizes partialSign without retaining aliases', () => {
    const source = [{ cbor: '00' }, { cbor: '01', partialSign: true }];
    const parsed = parseCip103SignRequest(source);

    source[0].cbor = '02';
    source.push({ cbor: '03' });

    expect(parsed).toEqual([
      { cbor: '00', partialSign: false },
      { cbor: '01', partialSign: true },
    ]);
    expect(Object.isFrozen(parsed)).toBe(true);
    expect(parsed.every(Object.isFrozen)).toBe(true);
    expect(parseCip103SubmitRequest(['00'])).toEqual(['00']);
  });

  it('rejects malformed shape, limits, and accessors through the frozen schema', () => {
    const getter = jest.fn(() => '00');
    const accessor = {};
    Object.defineProperty(accessor, 'cbor', { enumerable: true, get: getter });

    [
      [],
      Array(51).fill({ cbor: '00' }),
      [{ cbor: '0' }],
      [{ cbor: '00', partialSign: 'true' }],
      [{ cbor: '00', extra: true }],
      [accessor],
      [{ cbor: '00'.repeat(65_537) }],
    ].forEach((value) => expectInvalid(() => parseCip103SignRequest(value)));
    expectInvalid(() => parseCip103SubmitRequest([]));
    expect(getter).not.toHaveBeenCalled();
  });
});
