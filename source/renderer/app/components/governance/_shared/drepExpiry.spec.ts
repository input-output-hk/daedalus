import {
  epochsToDays,
  isInactiveSoon,
  INACTIVE_SOON_EPOCHS,
} from './drepExpiry';

// Preprod and mainnet: 432,000 slots of one second, so an epoch is five days.
const EPOCH_LENGTH = 432000;
const SLOT_LENGTH = 1;

describe('isInactiveSoon', () => {
  it('fires at the threshold and below', () => {
    expect(isInactiveSoon(INACTIVE_SOON_EPOCHS)).toBe(true);
    expect(isInactiveSoon(3)).toBe(true);
    expect(isInactiveSoon(0)).toBe(true);
  });

  it('does not fire above the threshold', () => {
    // dRepActivity is 20 epochs, so 12 remaining is 60 of a DRep's 100 days
    // and is not "soon" by any reading.
    expect(isInactiveSoon(INACTIVE_SOON_EPOCHS + 1)).toBe(false);
    expect(isInactiveSoon(12)).toBe(false);
    expect(isInactiveSoon(20)).toBe(false);
  });

  it('treats an unknown remaining count as not lapsing', () => {
    expect(isInactiveSoon(null)).toBe(false);
    expect(isInactiveSoon(undefined)).toBe(false);
  });

  it('uses the same threshold the directory filter uses', () => {
    expect(INACTIVE_SOON_EPOCHS).toBe(6);
  });
});

describe('epochsToDays', () => {
  it('converts using the chain epoch length rather than an assumed five days', () => {
    expect(epochsToDays(6, EPOCH_LENGTH, SLOT_LENGTH)).toBe(30);
    expect(epochsToDays(1, EPOCH_LENGTH, SLOT_LENGTH)).toBe(5);
    // A one-day epoch, as on preview, must not be reported as five.
    expect(epochsToDays(6, 86400, 1)).toBe(6);
  });

  it('rounds to whole days', () => {
    expect(epochsToDays(1, 100000, 1)).toBe(1);
  });

  it('returns null when the network parameters have not loaded', () => {
    expect(epochsToDays(6, null, SLOT_LENGTH)).toBeNull();
    expect(epochsToDays(6, EPOCH_LENGTH, null)).toBeNull();
    expect(epochsToDays(null, EPOCH_LENGTH, SLOT_LENGTH)).toBeNull();
  });
});
