import React, { useEffect, useState } from 'react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './DRepFacetNumber.scss';

interface Props {
  label: string;
  value: number;
  min: number;
  max: number;
  step: number;
  suffix?: string;
  decrementLabel: string;
  incrementLabel: string;
  onChange: (next: number) => void;
}

const clamp = (n: number, min: number, max: number) =>
  Math.min(max, Math.max(min, n));

// Stepping in fractions accumulates error: 1.5 + 0.5 is exact, but a tenth of
// a step is not, and a few presses drift into a value nobody typed.
const round = (n: number) => Math.round(n * 100) / 100;

/**
 * A bounded number, nudged by its own buttons or typed into directly.
 *
 * The buttons are the point rather than decoration. A bare field asks a reader
 * to work out what a valid entry looks like, where a stepper says the value is
 * one to nudge and shows the size of a nudge by moving it. They are ours
 * rather than the browser's: Chromium reveals its own only on hover, which
 * makes it useless as a signal, and its arrows are barely stylable.
 *
 * The two ways of setting a value commit differently, because they mean
 * different things. A press is a decision, so it applies at once. Typing is a
 * value under construction, so it applies when the field is committed: writing
 * "12" on the way to "12.5" would otherwise redraw the list for 12 first, and
 * a bound applied per keystroke rewrites "0." to the minimum under the caret.
 *
 * Polymorph's controls are being revisited for accessibility separately; this
 * one is deliberately outside that set rather than an oversight, and its
 * buttons carry their own labels in the meantime.
 */
function DRepFacetNumber({
  label,
  value,
  min,
  max,
  step,
  suffix,
  decrementLabel,
  incrementLabel,
  onChange,
}: Props) {
  const [draft, setDraft] = useState(String(value));

  useEffect(() => {
    setDraft(String(value));
  }, [value]);

  const commit = () => {
    const parsed = Number(draft);
    if (draft.trim() === '' || Number.isNaN(parsed)) {
      setDraft(String(value));
      return;
    }
    const bounded = round(clamp(parsed, min, max));
    setDraft(String(bounded));
    if (bounded !== value) onChange(bounded);
  };

  const nudge = (direction: 1 | -1) => {
    const next = round(clamp(value + step * direction, min, max));
    if (next !== value) onChange(next);
  };

  return (
    <div className={styles.facet}>
      <Input
        className={styles.input}
        label={label}
        value={draft}
        onChange={(next: string) => setDraft(next)}
        onBlur={commit}
        onKeyDown={(event: React.KeyboardEvent<HTMLInputElement>) => {
          // Arrow keys reach the same buttons rather than the browser's
          // stepper, so keyboard and pointer land on the same value.
          if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
            event.preventDefault();
            nudge(event.key === 'ArrowUp' ? 1 : -1);
            return;
          }
          if (event.key === 'Enter') commit();
        }}
        skin={InputSkin}
      />
      {suffix && (
        <span className={styles.suffix} aria-hidden="true">
          {suffix}
        </span>
      )}
      <span className={styles.steppers}>
        <button
          type="button"
          className={styles.stepper}
          aria-label={decrementLabel}
          disabled={value <= min}
          onClick={() => nudge(-1)}
        >
          −
        </button>
        <button
          type="button"
          className={styles.stepper}
          aria-label={incrementLabel}
          disabled={value >= max}
          onClick={() => nudge(1)}
        >
          +
        </button>
      </span>
    </div>
  );
}

export default DRepFacetNumber;
