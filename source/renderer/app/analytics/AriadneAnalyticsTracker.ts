import {
  ConsentView,
  EventMessage,
  WalletFlags,
  eventIntent,
  EventIntent,
  FunnelName,
} from '../../../common/analytics/contract';
import { AnalyticsTracker, EventCategories } from './types';

export class AriadneAnalyticsTracker implements AnalyticsTracker {
  private epoch = 0;
  private view: ConsentView | null = null;
  private sending = false;
  private enabling: Promise<void> | null = null;
  private attemptCounter = Date.now() * 1000;
  private funnelPending = 0;
  private funnelChain: Promise<unknown> = Promise.resolve();
  private flags: () => WalletFlags | null = () => null;
  constructor(
    private getConsent: () => Promise<ConsentView>,
    private send: (event: EventMessage) => Promise<boolean>,
    private now = Date.now
  ) {}
  setWalletSnapshot(read: () => WalletFlags | null) {
    this.flags = read;
  }
  enableTracking(): Promise<void> {
    if (this.enabling) return this.enabling;
    const epoch = this.epoch;
    const work = this.getConsent()
      .then((view) => {
        if (epoch === this.epoch)
          this.view = view?.enabled && view.status === 'ACCEPTED' ? view : null;
      })
      .catch(() => {
        if (epoch === this.epoch) this.view = null;
      })
      .finally(() => {
        if (this.enabling === work) this.enabling = null;
      });
    this.enabling = work;
    return work;
  }
  disableTracking() {
    this.epoch++;
    this.view = null;
    this.enabling = null;
  }
  sendPageNavigationEvent(page: string) {
    this.dispatch(eventIntent('page_view', page));
  }
  beginFunnel(flow: FunnelName) {
    const view = this.view;
    const epoch = this.epoch;
    if (!view) return undefined;
    let flags: WalletFlags | null;
    try {
      flags = this.flags();
    } catch {
      return undefined;
    }
    if (!flags) return undefined;
    const attempt = ++this.attemptCounter;
    let terminal = false;
    const send = (stage: 'started' | 'completed' | 'cancelled') => {
      if (
        this.funnelPending >= 8 ||
        epoch !== this.epoch ||
        view.generation !== this.view?.generation
      )
        return;
      const message: EventMessage = {
        type: 'funnel_step',
        action: flow,
        stage,
        attempt,
        ...flags,
        generation: view.generation,
        ts: new Date(this.now()).toISOString(),
      };
      this.funnelPending++;
      this.funnelChain = this.funnelChain
        .then(() => {
          if (epoch !== this.epoch || view.generation !== this.view?.generation)
            return false;
          return this.send(message);
        })
        .catch(() => false)
        .finally(() => {
          this.funnelPending--;
        });
    };
    send('started');
    return {
      complete: () => {
        if (!terminal) {
          terminal = true;
          send('completed');
        }
      },
      cancel: () => {
        if (!terminal) {
          terminal = true;
          send('cancelled');
        }
      },
    };
  }
  sendEvent(
    category: EventCategories,
    action: string,
    label?: string,
    _value?: number
  ) {
    this.dispatch(eventIntent('custom_event', action, category, label));
  }
  private dispatch(intent: EventIntent | null) {
    if (!intent || !this.view || this.sending) return;
    try {
      // Synchronous snapshot of already loaded wallet-store metadata. No wallet
      // API work, wallet records, identifiers or dynamic labels leave this process.
      const flags = this.flags();
      if (!flags) return;
      const message: EventMessage = {
        ...intent,
        ...flags,
        generation: this.view.generation,
        ts: new Date(this.now()).toISOString(),
      };
      this.sending = true;
      this.send(message)
        .catch(() => false)
        .finally(() => {
          this.sending = false;
        });
    } catch {
      this.sending = false;
    } // Telemetry never changes a wallet outcome.
  }
}
