import { formatLogMessage } from './logging';

const address = `addr1${'q'.repeat(98)}`;

describe('formatLogMessage', () => {
  it('redacts addresses in a text message', () => {
    expect(formatLogMessage(`Invalid receiver ${address}`)).toBe(
      'Invalid receiver [redacted-address]'
    );
  });

  it('reduces an error to its name and code', () => {
    const error = Object.assign(
      new Error(`spawnSync cardano-address ENOENT ${address}`),
      { code: 'ENOENT' }
    );

    expect(formatLogMessage(error)).toBe('Error ENOENT');
  });

  it('reduces an error without a code to its name', () => {
    expect(
      formatLogMessage(
        new TypeError("Cannot read properties of null (reading 'toString')")
      )
    ).toBe('TypeError');
  });

  it('converts other values to text', () => {
    expect(formatLogMessage(undefined)).toBe('undefined');
    expect(formatLogMessage(42)).toBe('42');
  });
});
