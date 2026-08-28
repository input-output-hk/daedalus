import { action, computed, observable, runInAction } from 'mobx';
import type {
  CollateralRendererRequest,
  CollateralSnapshot,
} from '../../../common/types/collateral.types';
import { dappCollateralChannel } from '../ipc/collateral';
import Store from './lib/Store';

export default class CollateralStore extends Store {
  @observable snapshot?: CollateralSnapshot;
  @observable isLoading = false;
  @observable preparationFormActive = false;
  @observable actionFailed = false;
  private generation = 0;

  @computed
  get state() {
    return this.snapshot?.preference.state ?? 'checking';
  }

  spendsPreference(
    inputs: readonly Readonly<{ id: string; index: number }>[]
  ): boolean {
    const preferred = this.snapshot?.preference.preferredInputs ?? [];
    return preferred.some(({ transactionId, index }) =>
      inputs.some(
        (input) => input.id === transactionId && input.index === index
      )
    );
  }

  @action.bound
  async refresh(): Promise<void> {
    await this.request({ type: 'snapshot' });
  }

  @action.bound
  async prepare(): Promise<void> {
    const snapshot = await this.request({ type: 'prepare' });
    const wallet = this.stores.wallets.activeDappWallet;
    if (snapshot?.preference.state === 'preparing' && wallet) {
      this.preparationFormActive = true;
      this.actions.router.goToRoute.trigger({
        route: this.stores.wallets.getWalletRoute(wallet.id, 'send'),
      });
    }
  }

  @action.bound
  async trackPreparation(transactionId: string): Promise<void> {
    this.preparationFormActive = false;
    await this.request({ type: 'track-preparation', transactionId });
  }

  @action.bound
  async cancelPreparation(): Promise<void> {
    this.preparationFormActive = false;
    await this.request({ type: 'cancel-preparation' });
  }

  @action.bound
  async clear(): Promise<void> {
    await this.request({ type: 'clear' });
  }

  @action.bound
  async repair(): Promise<void> {
    await this.request({ type: 'repair' });
  }

  teardown(): void {
    ++this.generation;
    this.snapshot = undefined;
    this.isLoading = false;
    this.preparationFormActive = false;
    super.teardown();
  }

  private async request(
    request: CollateralRendererRequest
  ): Promise<CollateralSnapshot | undefined> {
    const generation = ++this.generation;
    runInAction('CollateralStore::startRequest', () => {
      this.isLoading = true;
      this.actionFailed = false;
    });
    try {
      const snapshot = await dappCollateralChannel.request(request);
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
