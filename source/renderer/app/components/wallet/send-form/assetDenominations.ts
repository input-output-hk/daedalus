/**
 * The denomination an open amount field is being read in.
 *
 * The amount that gets signed is a pure function of the display string, with no
 * record of the denomination it was typed in: the send form computes it as
 * `formattedAmountToNaturalUnits(field.value)`, and that helper works by
 * deleting separators. The decimal places for an asset now arrive
 * asynchronously, so without this the meaning of what is already on screen can
 * change while the user is looking at it. A field holding `1500000` typed as raw
 * units, resolved to six decimal places and then touched once, submits
 * `1500000000000`.
 *
 * Two rules, and they are the condition the whole optimistic-decimals design was
 * accepted under:
 *
 * 1. The decimal places for an asset are snapshotted when its row is added to
 *    the form. The row renders and validates against the snapshot and never
 *    against the live value.
 * 2. If a resolution changes the decimal places while that row's field holds
 *    something, the field is cleared and the row says so. If the field is empty,
 *    the snapshot moves silently and nothing is said.
 *
 * Clearing is the only safe action. Once the denomination moves, the digits on
 * screen are ambiguous and neither reading can be assumed to be the one the user
 * meant.
 */

/**
 * The part of a form field this needs. `mobx-react-form`'s `Field` satisfies it,
 * and so can a test, which is what lets the Cucumber scenarios drive the same
 * code the form runs rather than deciding for themselves.
 */
export type DenominationField = {
  value: unknown;
  clear: () => void;
};

export type AssetDenominationRow = {
  uniqueId: string;
  currentDecimals?: number | null;
  field?: DenominationField | null;
};

export type DenominationReconciliation = {
  /** Rows whose field held an amount and was cleared. Each of these is told. */
  cleared: Array<string>;
  /** Rows whose snapshot moved with nothing on screen to invalidate. */
  adopted: Array<string>;
};

/**
 * Both spellings of "not known" are one state. Nothing else is normalised: a
 * move between no decimal places and zero decimal places puts the same digits on
 * screen today, and treating the two as equal would suppress a clear the rule
 * calls for. The rule errs toward clearing, so this does too.
 */
const snapshotValue = (decimals?: number | null): number | null =>
  typeof decimals === 'number' ? decimals : null;

const holdsAnAmount = (field?: DenominationField | null): boolean => {
  if (!field) return false;
  const { value } = field;
  if (value === null || value === undefined) return false;
  return String(value).length > 0;
};

export class AssetDenominations {
  private _snapshots: Map<string, number | null> = new Map();

  /** Taken when a row is added. Re-taking it for a row already held is a no-op. */
  snapshot(uniqueId: string, decimals?: number | null): void {
    if (this._snapshots.has(uniqueId)) return;
    this._snapshots.set(uniqueId, snapshotValue(decimals));
  }

  /** What the row is denominated in. Null is raw units. */
  decimalsFor(uniqueId: string): number | null {
    const snapshotted = this._snapshots.get(uniqueId);
    return snapshotted === undefined ? null : snapshotted;
  }

  forget(uniqueId: string): void {
    this._snapshots.delete(uniqueId);
  }

  /** Every row is gone, so every snapshot is. */
  clear(): void {
    this._snapshots.clear();
  }

  /**
   * Applies both rules to the rows as they stand now, clearing the fields that
   * have to be cleared, and answers with what happened.
   *
   * The snapshot moves in both branches. A field cleared under the old
   * denomination is re-entered under the new one, so a row that keeps the old
   * snapshot after being cleared would invite the user to make the same mistake
   * a second time.
   */
  reconcile(rows: Array<AssetDenominationRow>): DenominationReconciliation {
    const cleared: Array<string> = [];
    const adopted: Array<string> = [];

    rows.forEach(({ uniqueId, currentDecimals, field }) => {
      const next = snapshotValue(currentDecimals);

      if (!this._snapshots.has(uniqueId)) {
        this._snapshots.set(uniqueId, next);
        return;
      }

      if (this._snapshots.get(uniqueId) === next) return;

      this._snapshots.set(uniqueId, next);

      if (holdsAnAmount(field)) {
        field.clear();
        cleared.push(uniqueId);
        return;
      }

      adopted.push(uniqueId);
    });

    return { cleared, adopted };
  }
}
