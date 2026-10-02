import { computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import { ConsentStatus, ConsentView } from '../../../common/analytics/contract';

// One renderer snapshot. Main remains authoritative for persistence and dispatch.
export default class AnalyticsConsentStore extends Store {
  @observable.ref view: ConsentView | null = null;
  @observable saving = false;
  @observable saveFailed = false;
  private change = 0;
  private loadChange = 0;
  private disposed = false;

  @computed get trackingView(): ConsentView | null {
    return this.disposed || this.saving || this.saveFailed ? null : this.view;
  }

  setup() {
    this.load();
  }

  async load() {
    // A refresh cannot restore an earlier acceptance during/after a failed revoke.
    if (this.disposed || this.saving || this.saveFailed) return;
    const { change } = this;
    const loadChange = ++this.loadChange;
    this.analytics.disableTracking();
    try {
      const view = await this.api.analyticsConsent.get();
      if (change !== this.change || loadChange !== this.loadChange) return;
      runInAction(() => {
        this.view = view;
      });
      await this.analytics.enableTracking();
    } catch {
      if (change !== this.change || loadChange !== this.loadChange) return;
      this.analytics.disableTracking();
      runInAction(() => {
        this.view = null;
      });
    }
  }

  async save(status: ConsentStatus): Promise<boolean> {
    if (this.disposed) return false;
    const change = ++this.change;
    this.analytics.disableTracking();
    runInAction(() => {
      // Retain the last acknowledgement and availability so a failed write can be retried.
      this.saving = true;
      this.saveFailed = false;
    });
    try {
      // Never coalesce writes: a revoke must reach main even during acceptance.
      const view = await this.api.analyticsConsent.set(status);
      if (change !== this.change) return false;
      runInAction(() => {
        this.view = view;
        this.saving = false;
      });
      if (view.enabled && view.status === 'ACCEPTED')
        await this.analytics.enableTracking();
      return change === this.change;
    } catch {
      if (change === this.change)
        runInAction(() => {
          this.saveFailed = true;
        });
      return false;
    } finally {
      if (change === this.change)
        runInAction(() => {
          this.saving = false;
        });
    }
  }

  teardown() {
    this.disposed = true;
    this.change++;
    this.analytics.disableTracking();
    runInAction(() => {
      this.view = null;
    });
    super.teardown();
  }
}
