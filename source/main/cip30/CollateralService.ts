import { randomUUID } from 'crypto';
import { controlledCip30Utxos } from '../../common/cardano/cip30Serialization';
import type { Cip30Utxo } from '../../common/cardano/cip30Serialization';
import {
  isCip30CollateralCandidate,
  selectCip30Collateral,
} from '../../common/cardano/collateralSelection';
import { reconcileTransactionContext } from '../../common/cardano/transactionContext';
import type {
  Cip30WalletNetwork,
  Cip30WalletOutpoint,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../common/cip30/executor';
import {
  COLLATERAL_PREFERENCE_SCHEMA_VERSION,
  DEFAULT_COLLATERAL_TARGET_LOVELACE,
  CollateralInput,
  CollateralPreference,
  CollateralPreferenceRecord,
  CollateralPreparationOperation,
  CollateralSnapshot,
  CollateralState,
} from '../../common/types/collateral.types';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import { CollateralPreferenceStore } from './CollateralPreferenceStore';

const DEFAULT_TARGET_CBOR = '1a004c4b40';
type ExecuteWallet = (
  request: Cip30WalletRequest
) => Promise<Cip30WalletResponse>;

const sameInput = (
  left: CollateralInput,
  right: Readonly<{ transactionId: string; index: number }>
) => left.transactionId === right.transactionId && left.index === right.index;

const preference = (
  record: CollateralPreferenceRecord,
  state: CollateralState
): CollateralPreference => Object.freeze({ ...record, state });

type PreparationEntry = {
  readonly preparation: CollateralPreparationOperation;
  readonly lease: DappRouteLease;
  transactionId?: string;
  initialized: boolean;
  initialization: Promise<void>;
  unregister?: () => void;
};

export class CollateralService {
  private readonly preparing = new Map<string, PreparationEntry>();

  public constructor(
    private readonly records: CollateralPreferenceStore,
    private readonly executeWallet: ExecuteWallet,
    private readonly network: Cip30WalletNetwork,
    private readonly sourceRevision: string
  ) {}

  public async snapshot(lease: DappRouteLease): Promise<CollateralSnapshot> {
    this.assertLease(lease);
    const key = this.key(lease);
    const record = this.records.get(lease.walletId, lease.networkGenesis);
    const preparationAtStart = this.preparing.get(key);
    if (this.records.isCorrupt)
      return this.project(lease, record, 'stale', true);

    const response = await this.executeWallet({
      operation: 'context',
      walletId: lease.walletId,
      network: this.network,
      sourceRevision: this.sourceRevision,
    });
    if (response.status !== 'fulfilled' || response.operation !== 'context')
      throw new Error('Collateral context unavailable');
    const context = reconcileTransactionContext(response.value, {
      walletId: lease.walletId,
      network: this.network,
      transactions: [],
    });
    const utxos = controlledCip30Utxos(context);
    if (preparationAtStart && this.preparing.get(key) !== preparationAtStart)
      throw new Error('Collateral preparation is not active');

    if (this.preparing.has(key)) {
      const transactionId = this.preparing.get(key)?.transactionId;
      const selected =
        transactionId === undefined
          ? null
          : this.select(
              utxos.filter(
                ({ context: output }) =>
                  output.outpoint.transactionId === transactionId &&
                  output.pendingState === 'none'
              ),
              context.maxCollateralInputs
            );
      if (selected) {
        const adopted = this.records.put({
          walletId: lease.walletId,
          networkGenesis: lease.networkGenesis,
          targetLovelace: DEFAULT_COLLATERAL_TARGET_LOVELACE,
          preferredInputs: selected,
          generation: (record?.generation ?? 0) + 1,
        });
        this.removePreparation(key);
        return this.project(lease, adopted, 'ready');
      }
      const entry = this.preparing.get(key);
      return this.project(entry?.lease ?? lease, record, 'preparing');
    }

    if (record) {
      const pendingState = this.pendingState(
        record,
        context.pendingTransactions
      );
      if (pendingState) return this.project(lease, record, pendingState);
      if (this.recordReady(record, utxos))
        return this.project(lease, record, 'ready');
      const history = await this.executeWallet({
        operation: 'collateral-history',
        walletId: lease.walletId,
        network: this.network,
        sourceRevision: this.sourceRevision,
        preferredInputs: record.preferredInputs,
      });
      if (
        history.status === 'fulfilled' &&
        history.operation === 'collateral-history' &&
        history.value.transactions.some(
          (transaction) =>
            transaction.status === 'in_ledger' &&
            transaction.scriptValidity === 'invalid' &&
            transaction.collateralInputs.some((input) =>
              record.preferredInputs.some((preferred) =>
                sameInput(preferred, input)
              )
            )
        )
      )
        return this.project(lease, record, 'charged');
      return this.project(lease, record, 'stale');
    }

    const selected = this.select(utxos, context.maxCollateralInputs);
    if (selected) {
      const adopted = this.records.put({
        walletId: lease.walletId,
        networkGenesis: lease.networkGenesis,
        targetLovelace: DEFAULT_COLLATERAL_TARGET_LOVELACE,
        preferredInputs: selected,
        generation: 1,
      });
      return this.project(lease, adopted, 'ready');
    }
    return this.project(lease, undefined, 'not-ready');
  }

  public async prepare(
    lease: DappRouteLease,
    registerInvalidation?: (invalidate: () => void) => () => void
  ): Promise<CollateralSnapshot> {
    this.assertLease(lease);
    const key = this.key(lease);
    let entry = this.preparing.get(key);
    if (!entry) {
      const created: PreparationEntry = {
        preparation: Object.freeze({
          operationId: randomUUID(),
          walletId: lease.walletId,
          networkGenesis: lease.networkGenesis,
        }),
        lease: Object.freeze({ ...lease }),
        initialized: false,
        initialization: Promise.resolve(),
      };
      entry = created;
      this.preparing.set(key, created);
      try {
        if (registerInvalidation) {
          const unregister = registerInvalidation(() => {
            if (this.preparing.get(key) === created)
              this.removePreparation(key);
          });
          if (this.preparing.get(key) !== created) {
            unregister();
            throw new Error('Collateral preparation is not active');
          }
          created.unregister = unregister;
        }
        created.initialization = this.snapshot(created.lease).then(
          (snapshot) => {
            if (snapshot.corrupt)
              throw new Error('Collateral preferences require repair');
            if (this.preparing.get(key) !== created)
              throw new Error('Collateral preparation is not active');
            created.initialized = true;
          }
        );
      } catch (error) {
        if (this.preparing.get(key) === created) this.removePreparation(key);
        throw error;
      }
    }
    try {
      await entry.initialization;
    } catch (error) {
      if (this.preparing.get(key) === entry) this.removePreparation(key);
      throw error;
    }
    if (this.preparing.get(key) !== entry)
      throw new Error('Collateral preparation is not active');
    return this.project(
      entry.lease,
      this.records.get(
        entry.preparation.walletId,
        entry.preparation.networkGenesis
      ),
      'preparing'
    );
  }

  public trackPreparation(
    preparation: CollateralPreparationOperation,
    transactionId: string
  ): CollateralSnapshot {
    const entry = this.requirePreparation(preparation);
    if (!/^[0-9a-f]{64}$/u.test(transactionId))
      throw new Error('Invalid collateral preparation transaction');
    if (
      entry.transactionId !== undefined &&
      entry.transactionId !== transactionId
    )
      throw new Error('Invalid collateral preparation transaction');
    entry.transactionId = transactionId;
    return this.project(
      entry.lease,
      this.records.get(preparation.walletId, preparation.networkGenesis),
      'preparing'
    );
  }

  public preferredInputs(walletId: string): readonly Cip30WalletOutpoint[] {
    if (this.records.isCorrupt)
      throw new Error('Collateral preferences require repair');
    return (
      this.records.get(walletId, this.network.genesisHash)?.preferredInputs ??
      []
    );
  }

  public spendsPreference(
    lease: DappRouteLease,
    inputs: readonly Cip30WalletOutpoint[]
  ): boolean {
    this.assertLease(lease);
    return this.preferredInputs(lease.walletId).some((preferred) =>
      inputs.some((input) => sameInput(preferred, input))
    );
  }

  public cancelPreparation(
    preparation: CollateralPreparationOperation
  ): CollateralSnapshot {
    const entry = this.requirePreparation(preparation);
    this.removePreparation(this.key(entry.lease));
    return this.project(
      entry.lease,
      this.records.get(preparation.walletId, preparation.networkGenesis),
      'checking'
    );
  }

  public removeWallet(walletId: string): void {
    for (const [key, entry] of this.preparing)
      if (entry.preparation.walletId === walletId) this.removePreparation(key);
  }

  public pruneWallets(walletIds: readonly string[]): void {
    const retained = new Set(walletIds);
    for (const [key, entry] of this.preparing)
      if (!retained.has(entry.preparation.walletId))
        this.removePreparation(key);
  }

  private requirePreparation(
    preparation: CollateralPreparationOperation
  ): PreparationEntry {
    const entry = this.preparing.get(this.key(preparation));
    if (
      preparation.networkGenesis !== this.network.genesisHash ||
      !entry?.initialized ||
      entry.preparation.operationId !== preparation.operationId ||
      entry.preparation.walletId !== preparation.walletId ||
      entry.preparation.networkGenesis !== preparation.networkGenesis
    )
      throw new Error('Collateral preparation is not active');
    return entry;
  }

  private removePreparation(key: string): void {
    const entry = this.preparing.get(key);
    this.preparing.delete(key);
    entry?.unregister?.();
  }

  public clear(lease: DappRouteLease): CollateralSnapshot {
    this.assertLease(lease);
    this.removePreparation(this.key(lease));
    this.records.clear(lease.walletId, lease.networkGenesis);
    return this.project(lease, undefined, 'not-ready');
  }

  public repair(lease: DappRouteLease): CollateralSnapshot {
    this.assertLease(lease);
    for (const key of this.preparing.keys()) this.removePreparation(key);
    this.records.repair();
    return this.project(lease, undefined, 'not-ready');
  }

  private pendingState(
    record: CollateralPreferenceRecord,
    pending: readonly Readonly<{
      normalInputs: readonly Readonly<{
        transactionId: string;
        index: number;
      }>[];
      collateralInputs: readonly Readonly<{
        transactionId: string;
        index: number;
      }>[];
    }>[]
  ): 'will-be-spent' | 'in-use' | undefined {
    if (
      pending.some((transaction) =>
        transaction.normalInputs.some((input) =>
          record.preferredInputs.some((preferred) =>
            sameInput(preferred, input)
          )
        )
      )
    )
      return 'will-be-spent';
    if (
      pending.some((transaction) =>
        transaction.collateralInputs.some((input) =>
          record.preferredInputs.some((preferred) =>
            sameInput(preferred, input)
          )
        )
      )
    )
      return 'in-use';
    return undefined;
  }

  private select(
    utxos: readonly Cip30Utxo[],
    maxCollateralInputs: number | undefined
  ): readonly CollateralInput[] | null {
    if (maxCollateralInputs === undefined) return null;
    const selected = selectCip30Collateral(
      utxos,
      DEFAULT_TARGET_CBOR,
      maxCollateralInputs
    );
    if (!selected) return null;
    const selectedSet = new Set(selected);
    return Object.freeze(
      utxos
        .filter(({ context: output }) => selectedSet.has(output.unspentCbor))
        .map(({ context: output }) => Object.freeze({ ...output.outpoint }))
    );
  }

  private recordReady(
    record: CollateralPreferenceRecord,
    utxos: readonly Cip30Utxo[]
  ): boolean {
    const selected = record.preferredInputs.map((input) =>
      utxos.find(({ context }) => sameInput(input, context.outpoint))
    );
    return (
      selected.every(
        (utxo): utxo is Cip30Utxo => !!utxo && isCip30CollateralCandidate(utxo)
      ) &&
      selected.reduce((total, utxo) => total + utxo.value.coin, BigInt(0)) >=
        BigInt(record.targetLovelace)
    );
  }

  private project(
    lease: DappRouteLease,
    record: CollateralPreferenceRecord | undefined,
    state: CollateralState,
    corrupt = false
  ): CollateralSnapshot {
    const projected =
      record ??
      Object.freeze({
        schemaVersion: COLLATERAL_PREFERENCE_SCHEMA_VERSION,
        walletId: lease.walletId,
        networkGenesis: lease.networkGenesis,
        targetLovelace: DEFAULT_COLLATERAL_TARGET_LOVELACE,
        preferredInputs: Object.freeze([]),
        generation: 0,
      });
    return Object.freeze({
      corrupt,
      preference: preference(projected, state),
      ...(this.preparing.get(this.key(lease))?.initialized
        ? { preparation: this.preparing.get(this.key(lease))?.preparation }
        : {}),
    });
  }

  private assertLease(lease: DappRouteLease): void {
    if (
      lease.networkGenesis !== this.network.genesisHash ||
      lease.walletId.length === 0
    )
      throw new Error('Collateral route mismatch');
  }

  private key(
    lease: Readonly<{ walletId: string; networkGenesis: string }>
  ): string {
    return `${lease.walletId}\0${lease.networkGenesis}`;
  }
}
