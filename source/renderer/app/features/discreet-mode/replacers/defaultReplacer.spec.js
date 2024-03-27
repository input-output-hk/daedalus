'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const defaultReplacer_1 = require('./defaultReplacer');
describe('Discreet mode default replacer', () => {
  const symbol = '***';
  const value = 'test';
  it('should replace the given value with the sensitive data symbol', () => {
    expect(
      (0, defaultReplacer_1.defaultReplacer)()(true, symbol, value)
    ).toEqual(symbol);
  });
  it('should keep the original value', () => {
    expect(
      (0, defaultReplacer_1.defaultReplacer)()(false, symbol, value)
    ).toEqual(value);
  });
});
//# sourceMappingURL=defaultReplacer.spec.js.map
