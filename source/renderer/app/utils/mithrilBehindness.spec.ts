import { isMithrilBehindnessKnown } from './mithrilBehindness';
import type { TipInfo } from '../api/network/types';

const tip = (epoch: unknown): TipInfo =>
  (({
    epoch,
    slot: 0,
    absoluteSlotNumber: 0,
  } as unknown) as TipInfo);

describe('isMithrilBehindnessKnown', () => {
  it('is false when the network tip is missing', () => {
    expect(isMithrilBehindnessKnown(tip(10), null)).toBe(false);
    expect(isMithrilBehindnessKnown(tip(10), undefined)).toBe(false);
  });

  it('is false when the local tip is missing', () => {
    expect(isMithrilBehindnessKnown(null, tip(10))).toBe(false);
    expect(isMithrilBehindnessKnown(undefined, tip(10))).toBe(false);
  });

  it('is false when either epoch is non-finite', () => {
    expect(isMithrilBehindnessKnown(tip(undefined), tip(10))).toBe(false);
    expect(isMithrilBehindnessKnown(tip(10), tip(null))).toBe(false);
    expect(isMithrilBehindnessKnown(tip(NaN), tip(10))).toBe(false);
    expect(isMithrilBehindnessKnown(tip(10), tip(Infinity))).toBe(false);
  });

  it('is true once both tips carry finite epochs', () => {
    expect(isMithrilBehindnessKnown(tip(5), tip(12))).toBe(true);
    // Equal epochs are still "known" — behind-ness is a separate figure.
    expect(isMithrilBehindnessKnown(tip(7), tip(7))).toBe(true);
    expect(isMithrilBehindnessKnown(tip(0), tip(0))).toBe(true);
  });
});
