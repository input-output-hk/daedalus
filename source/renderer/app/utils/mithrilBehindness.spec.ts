import {
  isMithrilBehindnessKnown,
  computeBehindByEpochs,
} from './mithrilBehindness';
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

describe('computeBehindByEpochs', () => {
  it('returns the positive epoch gap when both tips are finite', () => {
    expect(computeBehindByEpochs(tip(5), tip(12))).toBe(7);
  });

  it('returns 1 for a gap of exactly 1 (no plural clamp from below)', () => {
    expect(computeBehindByEpochs(tip(11), tip(12))).toBe(1);
  });

  it('returns undefined at a gap of 0 (node level — no Math.max(1) clamp)', () => {
    expect(computeBehindByEpochs(tip(7), tip(7))).toBeUndefined();
  });

  it('returns undefined when the gap is negative (node ahead)', () => {
    expect(computeBehindByEpochs(tip(12), tip(5))).toBeUndefined();
  });

  it('returns undefined when a tip is missing or non-finite', () => {
    expect(computeBehindByEpochs(tip(5), null)).toBeUndefined();
    expect(computeBehindByEpochs(tip(5), undefined)).toBeUndefined();
    expect(computeBehindByEpochs(null, tip(12))).toBeUndefined();
    expect(computeBehindByEpochs(undefined, tip(12))).toBeUndefined();
    expect(computeBehindByEpochs(tip(undefined), tip(12))).toBeUndefined();
    expect(computeBehindByEpochs(tip(5), tip(NaN))).toBeUndefined();
    expect(computeBehindByEpochs(tip(5), tip(Infinity))).toBeUndefined();
  });

  // #16 hybrid anchor (D-702b-10).
  it('#16 falls back to certifiedEpoch when networkTip is missing/non-finite', () => {
    expect(computeBehindByEpochs(tip(5), null, 12)).toBe(7);
    expect(computeBehindByEpochs(tip(5), undefined, 12)).toBe(7);
    expect(computeBehindByEpochs(tip(5), tip(null), 12)).toBe(7);
  });

  it('#16 prefers networkTip.epoch over certifiedEpoch when both are finite', () => {
    // networkTip is the live anchor; certifiedEpoch (the lagging beacon) is ignored.
    expect(computeBehindByEpochs(tip(5), tip(12), 9)).toBe(7);
    expect(computeBehindByEpochs(tip(5), tip(9), 12)).toBe(4);
  });

  it('#16 returns undefined when both networkTip and certifiedEpoch are absent', () => {
    expect(computeBehindByEpochs(tip(5), null)).toBeUndefined();
    expect(computeBehindByEpochs(tip(5), null, undefined)).toBeUndefined();
    expect(computeBehindByEpochs(tip(5), null, null)).toBeUndefined();
  });

  it('#16 returns undefined when certifiedEpoch is finite but <= localTip.epoch', () => {
    expect(computeBehindByEpochs(tip(12), null, 12)).toBeUndefined();
    expect(computeBehindByEpochs(tip(12), null, 5)).toBeUndefined();
  });

  it('#16 returns undefined when localTip.epoch is non-finite regardless of anchor', () => {
    expect(computeBehindByEpochs(tip(undefined), tip(12), 12)).toBeUndefined();
    expect(computeBehindByEpochs(null, null, 12)).toBeUndefined();
  });
});
