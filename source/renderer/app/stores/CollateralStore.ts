import { action, computed, observable, runInAction } from 'mobx';
import type { CollateralSnapshot } from '../../../common/types/collateral.types';
import { dappCollateralChannel } from '../ipc/collateral';
import Store from './lib/Store';

export default class CollateralStore extends Store {
  @observable snapshot?: CollateralSnapshot;
  @observable isLoading = false;
  @observable actionFailed = false;
  private generation = 0;

  @computed
  get state() {
    return this.snapshot?.preference.state ?? 'checking';
  }

  @action.bound
  async refresh(): Promise<void> {
    await this.request('snapshot');
  }

  @action.bound
  async prepare(): Promise<void> {
    const snapshot = await this.request('prepare');
    const wallet = this.stores.wallets.activeDappWallet;
    if (snapshot?.preference.state === 'preparing' && wallet)
      this.actions.router.goToRoute.trigger({
        route: this.stores.wallets.getWalletRoute(wallet.id, 'send'),
      });
  }

  @action.bound
  async cancelPreparation(): Promise<void> {
    await this.request('cancel-preparation');
  }

  @action.bound
  async clear(): Promise<void> {
    await this.request('clear');
  }

  @action.bound
  async repair(): Promise<void> {
    await this.request('repair');
  }

  teardown(): void {
    ++this.generation;
    this.snapshot = undefined;
    this.isLoading = false;
    super.teardown();
  }

  private async request(
    type: 'snapshot' | 'prepare' | 'cancel-preparation' | 'clear' | 'repair'
  ): Promise<CollateralSnapshot | undefined> {
    const generation = ++this.generation;
    runInAction('CollateralStore::startRequest', () => {
      this.isLoading = true;
      this.actionFailed = false;
    });
    try {
      const snapshot = await dappCollateralChannel.request({ type });
      if (generation !== this.generation) return undefined;
      runInAction('CollateralStore::receiveSnapshot', () => {
        this.snapshot = snapshot;
      });
      return snapshot;
    } catch {
      if (generation === this.generation)
        runInAction('CollateralStore::requestFailed', () => {
          this.actionFailed = true;
        });
      return undefined;
    } finally {
      if (generation === this.generation)
        runInAction('CollateralStore::finishRequest', () => {
          this.isLoading = false;
        });
    }
  }
}
