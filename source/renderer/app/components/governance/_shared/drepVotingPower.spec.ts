import BigNumber from 'bignumber.js';
import {
  formatVotingPower,
  getVotingPowerShare,
  HIGH_VOTING_POWER_THRESHOLD,
  isHighVotingPower,
} from './drepVotingPower';

const TOTAL = new BigNumber('5000000000000000'); // 5B ADA in lovelace

describe('getVotingPowerShare', () => {
  it('divides voting power by the DRep total', () => {
    expect(getVotingPowerShare(new BigNumber('100000000000000'), TOTAL)).toBe(
      0.02
    );
  });

  it('returns null when either side is missing', () => {
    expect(getVotingPowerShare(null, TOTAL)).toBeNull();
    expect(getVotingPowerShare(new BigNumber('1'), null)).toBeNull();
    expect(getVotingPowerShare(undefined, undefined)).toBeNull();
  });

  it('returns null rather than dividing by zero', () => {
    expect(
      getVotingPowerShare(new BigNumber('1'), new BigNumber(0))
    ).toBeNull();
  });
});

describe('isHighVotingPower', () => {
  it('fires at the threshold and above', () => {
    expect(isHighVotingPower(HIGH_VOTING_POWER_THRESHOLD)).toBe(true);
    expect(isHighVotingPower(0.5)).toBe(true);
  });

  it('does not fire below it', () => {
    expect(isHighVotingPower(0.0149)).toBe(false);
    expect(isHighVotingPower(0)).toBe(false);
    expect(isHighVotingPower(null)).toBe(false);
  });

  it('uses the Target15 threshold', () => {
    // CR10 of 15% across the ten largest DReps puts each at or below ~1.5%.
    expect(HIGH_VOTING_POWER_THRESHOLD).toBe(0.015);
  });
});

describe('formatVotingPower', () => {
  it('abbreviates across the whole range the directory shows', () => {
    // Three-digit millions: mainnet's largest DRep is in this range.
    expect(formatVotingPower(new BigNumber('565800000000000'))).toBe(
      '₳ 565.8M'
    );
    expect(formatVotingPower(new BigNumber('100000000000000'))).toBe(
      '₳ 100.0M'
    );
    expect(formatVotingPower(new BigNumber('4200000000000'))).toBe('₳ 4.2M');
    expect(formatVotingPower(new BigNumber('125000000000'))).toBe('₳ 125.0K');
    expect(formatVotingPower(new BigNumber('5000000000'))).toBe('₳ 5.0K');
    // Sub-thousand tail: no abbreviation, no decimals.
    expect(formatVotingPower(new BigNumber('940000000'))).toBe('₳ 940');
    expect(formatVotingPower(new BigNumber('1000000'))).toBe('₳ 1');
  });

  it('renders a dash when there is nothing to show', () => {
    expect(formatVotingPower(null)).toBe('—');
  });
});
