import type { AnalyticsTracker, CapturedAttempt } from './types';
import type { FunnelName } from '../../../common/analytics/contract';

/** A dialog visit, not a wallet or transaction identity. Never await telemetry. */
export class FunnelCapture {
  private attempt?: CapturedAttempt;
  private submitted = false;
  constructor(
    private tracker: AnalyticsTracker,
    private flow: FunnelName
  ) {}
  open() {
    this.close();
    this.submitted = false;
    try {
      this.attempt = this.tracker.beginFunnel?.(this.flow);
    } catch {
      this.attempt = undefined;
    }
  }
  submission() {
    this.submitted = true;
    return this.attempt;
  }
  current() {
    return this.attempt;
  }
  complete(captured: CapturedAttempt | undefined) {
    try {
      captured?.complete();
    } catch {
      /* Wallet operation remains independent. */
    }
    if (captured === this.attempt) this.attempt = undefined;
  }
  close() {
    // Once submission begins, dismissal/error cannot prove cancellation at the backend.
    try {
      if (!this.submitted) this.attempt?.cancel();
    } catch {
      /* Best effort. */
    }
    this.attempt = undefined;
  }
}
