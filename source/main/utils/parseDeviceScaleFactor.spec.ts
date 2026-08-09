import { parseDeviceScaleFactor } from './parseDeviceScaleFactor';

describe('parseDeviceScaleFactor', () => {
  it('accepts a fractional scale factor', () => {
    expect(parseDeviceScaleFactor('1.5')).toBe(1.5);
  });

  it('accepts an integer scale factor', () => {
    expect(parseDeviceScaleFactor('2')).toBe(2);
  });

  it('accepts a scale factor below 1', () => {
    expect(parseDeviceScaleFactor('0.75')).toBe(0.75);
  });

  it('rejects an unset value, leaving the detected scale factor in place', () => {
    expect(parseDeviceScaleFactor(undefined)).toBeNull();
  });

  it('rejects an empty value', () => {
    expect(parseDeviceScaleFactor('')).toBeNull();
  });

  it('rejects a non-numeric value', () => {
    expect(parseDeviceScaleFactor('abc')).toBeNull();
  });

  it('rejects zero, which would make the window unrenderable', () => {
    expect(parseDeviceScaleFactor('0')).toBeNull();
  });

  it('rejects a negative value', () => {
    expect(parseDeviceScaleFactor('-1')).toBeNull();
  });

  it('rejects a non-finite value', () => {
    expect(parseDeviceScaleFactor('Infinity')).toBeNull();
  });
});
