import { defaultReplacer } from './defaultReplacer';

describe('Discreet mode default replacer', () => {
  const symbol = '***';
  const value = 'test';
  it('should replace the given value with the sensitive data symbol', () => {
    expect(defaultReplacer()(true, symbol, value)).toEqual(symbol);
  });
  it('should keep the original value', () => {
    expect(defaultReplacer()(false, symbol, value)).toEqual(value);
  });
});
