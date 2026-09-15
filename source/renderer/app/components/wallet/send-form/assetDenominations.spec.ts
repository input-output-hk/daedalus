import { AssetDenominations } from './assetDenominations';

const SUBJECT = 'a'.repeat(56) + '42544544';
const OTHER = 'b'.repeat(56) + '42544545';

const field = (value: unknown) => {
  const state = { value, cleared: 0 };
  return {
    get value() {
      return state.value;
    },
    clear: () => {
      state.value = '';
      state.cleared += 1;
    },
    clearedTimes: () => state.cleared,
  };
};

describe('AssetDenominations', () => {
  it('answers with the snapshot a row was opened in', () => {
    const denominations = new AssetDenominations();
    denominations.snapshot(SUBJECT, 6);
    expect(denominations.decimalsFor(SUBJECT)).toBe(6);
  });

  it('answers raw units for a row it has never seen', () => {
    expect(new AssetDenominations().decimalsFor(SUBJECT)).toBeNull();
  });

  it('records both spellings of unknown as one state', () => {
    const fromUndefined = new AssetDenominations();
    fromUndefined.snapshot(SUBJECT, undefined);
    const fromNull = new AssetDenominations();
    fromNull.snapshot(SUBJECT, null);
    expect(fromUndefined.decimalsFor(SUBJECT)).toBe(
      fromNull.decimalsFor(SUBJECT)
    );
  });

  it('keeps the snapshot a row was opened in when it is taken again', () => {
    const denominations = new AssetDenominations();
    denominations.snapshot(SUBJECT, null);
    denominations.snapshot(SUBJECT, 6);
    expect(denominations.decimalsFor(SUBJECT)).toBeNull();
  });

  describe('reconcile', () => {
    it('clears a field holding an amount and reports the row', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      const amount = field('1500000');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: amount },
      ]);

      expect(amount.value).toBe('');
      expect(amount.clearedTimes()).toBe(1);
      expect(result.cleared).toEqual([SUBJECT]);
      expect(result.adopted).toEqual([]);
    });

    it('adopts the new denomination for a cleared row', () => {
      // Otherwise the user re-types into a field still denominated in the value
      // the application no longer believes, which is the original defect again.
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: field('1500000') },
      ]);
      expect(denominations.decimalsFor(SUBJECT)).toBe(6);
    });

    it('moves an empty row silently', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      const amount = field('');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: amount },
      ]);

      expect(amount.clearedTimes()).toBe(0);
      expect(result.cleared).toEqual([]);
      expect(result.adopted).toEqual([SUBJECT]);
      expect(denominations.decimalsFor(SUBJECT)).toBe(6);
    });

    it('treats a field holding null as empty', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: field(null) },
      ]);
      expect(result.cleared).toEqual([]);
      expect(result.adopted).toEqual([SUBJECT]);
    });

    it('does nothing when the denomination has not moved', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, 6);
      const amount = field('1.500000');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: amount },
      ]);

      expect(amount.value).toBe('1.500000');
      expect(result).toEqual({ cleared: [], adopted: [] });
    });

    it('clears on a cached value that changed on re-read, not only on a first resolution', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, 6);
      const amount = field('1.500000');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 2, field: amount },
      ]);

      expect(result.cleared).toEqual([SUBJECT]);
      expect(denominations.decimalsFor(SUBJECT)).toBe(2);
    });

    it('clears on a move from unknown to zero', () => {
      // Both put the field in raw units today, so the digits mean the same
      // thing either way. The rule is that a change in the decimal places
      // clears, and it is implemented as written: erring toward clearing costs
      // a retype, and erring the other way costs an amount.
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 0, field: field('1500000') },
      ]);
      expect(result.cleared).toEqual([SUBJECT]);
    });

    it('takes a first snapshot for a row it has not seen, without clearing', () => {
      const denominations = new AssetDenominations();
      const amount = field('1500000');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: amount },
      ]);

      expect(amount.clearedTimes()).toBe(0);
      expect(result).toEqual({ cleared: [], adopted: [] });
      expect(denominations.decimalsFor(SUBJECT)).toBe(6);
    });

    it('touches only the row whose denomination moved', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      denominations.snapshot(OTHER, 6);
      const moved = field('1500000');
      const untouched = field('1.500000');

      const result = denominations.reconcile([
        { uniqueId: SUBJECT, currentDecimals: 6, field: moved },
        { uniqueId: OTHER, currentDecimals: 6, field: untouched },
      ]);

      expect(result.cleared).toEqual([SUBJECT]);
      expect(untouched.value).toBe('1.500000');
    });

    it('forgets a removed row, so the same token reopens at whatever it is then', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, null);
      denominations.forget(SUBJECT);
      denominations.snapshot(SUBJECT, 6);
      expect(denominations.decimalsFor(SUBJECT)).toBe(6);
    });

    it('drops every snapshot when the form is reset', () => {
      const denominations = new AssetDenominations();
      denominations.snapshot(SUBJECT, 6);
      denominations.clear();
      expect(denominations.decimalsFor(SUBJECT)).toBeNull();
    });
  });
});
