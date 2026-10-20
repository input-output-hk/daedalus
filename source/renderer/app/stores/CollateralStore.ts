import { action, computed, observable, runInAction } from 'mobx';
import type {
  CollateralPreparationOperation,
  CollateralRendererRequest,
  CollateralSnapshot,
} from '../../../common/types/collateral.types';
import { dappCollateralChannel } from '../ipc/collateral';
import Store from './lib/Store';

const COLLATERAL_REQUEST_TIMEOUT_MS = 30_000;

export default class CollateralStore extends Store {
  @observable snapshot?: CollateralSnapshot;
  @observable.ref preparation?: CollateralPreparationOperation;
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
    if (!global.cip30Enabled) return false;
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
    const preparation = snapshot?.preparation;
    if (snapshot?.preference.state === 'preparing' && preparation) {
      this.preparation = Object.freeze({ ...preparation });
      this.preparationFormActive = true;
      this.actions.router.goToRoute.trigger({
        route: this.stores.wallets.getWalletRoute(preparation.walletId, 'send'),
      });
    } else if (snapshot?.preference.state === 'preparing') {
      this.actionFailed = true;
    }
  }

  @action.bound
  async trackPreparation(
    walletId: string,
    transactionId: string
  ): Promise<void> {
    if (!global.cip30Enabled) return;
    const preparation = this.preparation;
    if (!preparation || preparation.walletId !== walletId) {
      this.actionFailed = true;
      throw new Error('Collateral preparation does not match wallet');
    }
    this.preparationFormActive = false;
    await this.request({
      type: 'track-preparation',
      preparation,
      transactionId,
    });
  }

  @action.bound
  async cancelPreparation(): Promise<void> {
    if (!global.cip30Enabled || !this.preparation) return;
    const preparation = this.preparation;
    const snapshot = await this.request({
      type: 'cancel-preparation',
      preparation,
    });
    if (snapshot && this.preparation === preparation) {
      this.preparationFormActive = false;
      this.preparation = undefined;
    }
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
    const preparation = this.preparation;
    if (global.cip30Enabled && preparation)
      dappCollateralChannel
        .request({
          type: 'cancel-preparation',
          preparation,
        })
        .catch(() => undefined);
    ++this.generation;
    this.snapshot = undefined;
    this.preparation = undefined;
    this.isLoading = false;
    this.preparationFormActive = false;
    super.teardown();
  }

  private async request(
    request: CollateralRendererRequest
  ): Promise<CollateralSnapshot | undefined> {
    if (!global.cip30Enabled) return undefined;
    const generation = ++this.generation;
    runInAction('CollateralStore::startRequest', () => {
      this.isLoading = true;
      this.actionFailed = false;
    });
    let timer: number | undefined;
    try {
      const snapshot = await Promise.race([
        dappCollateralChannel.request(request),
        new Promise<never>((_resolve, reject) => {
          timer = window.setTimeout(
            () => reject(new Error('Collateral request timed out')),
            COLLATERAL_REQUEST_TIMEOUT_MS
          );
        }),
      ]);
      if (generation !== this.generation) return undefined;
      runInAction('CollateralStore::receiveSnapshot', () => {
        this.snapshot = snapshot;
        if (
          snapshot.preference.state === 'ready' &&
          this.preparation?.walletId === snapshot.preference.walletId &&
          this.preparation.networkGenesis === snapshot.preference.networkGenesis
        ) {
          this.preparation = undefined;
          this.preparationFormActive = false;
        }
      });
      return snapshot;
    } catch {
      if (generation === this.generation)
        runInAction('CollateralStore::requestFailed', () => {
          this.actionFailed = true;
        });
      return undefined;
    } finally {
      window.clearTimeout(timer);
      if (generation === this.generation)
        runInAction('CollateralStore::finishRequest', () => {
          this.isLoading = false;
        });
    }
  }
}
