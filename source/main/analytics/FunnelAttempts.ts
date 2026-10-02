import { FunnelIntent } from '../../common/analytics/contract';
import { analyticsUuid } from './uuid';

type Attempt = { id: string; flow: string; started: number };

// Prepare before queue admission; commit only after successful admission.
export class FunnelAttempts {
  private attempts = new Map<number, Attempt>();
  private lastAttempt = 0;

  prepare(
    input: FunnelIntent & { ts: string },
    now: number
  ): Attempt | undefined {
    for (const [key, value] of this.attempts)
      if (value.started + 30 * 60_000 < now) this.attempts.delete(key);
    const attempt = this.attempts.get(input.attempt);
    if (input.stage === 'started') {
      if (
        attempt ||
        input.attempt <= this.lastAttempt ||
        this.attempts.size >= 8
      )
        return undefined;
      return {
        id: analyticsUuid(),
        flow: input.action,
        started: Date.parse(input.ts),
      };
    }
    if (
      !attempt ||
      attempt.flow !== input.action ||
      Date.parse(input.ts) < attempt.started
    )
      return undefined;
    return attempt;
  }

  commit(input: FunnelIntent, attempt: Attempt) {
    if (input.stage === 'started') {
      this.lastAttempt = input.attempt;
      this.attempts.set(input.attempt, attempt);
    } else this.attempts.delete(input.attempt);
  }

  clear() {
    this.attempts.clear();
    this.lastAttempt = 0;
  }
}
