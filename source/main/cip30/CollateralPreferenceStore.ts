import fs from 'fs';
import path from 'path';
import {
  COLLATERAL_PREFERENCE_SCHEMA_VERSION,
  CollateralPreferenceRecord,
} from '../../common/types/collateral.types';

const ownKeysAre = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const object = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);
const text = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
const uint = (value: unknown): value is number =>
  Number.isSafeInteger(value) && Number(value) >= 0;
const transactionId = (value: unknown): value is string =>
  typeof value === 'string' && /^[0-9a-f]{64}$/u.test(value);

const parseRecord = (value: unknown): CollateralPreferenceRecord => {
  if (
    !object(value) ||
    !ownKeysAre(value, [
      'schemaVersion',
      'walletId',
      'networkGenesis',
      'targetLovelace',
      'preferredInputs',
      'generation',
    ]) ||
    value.schemaVersion !== COLLATERAL_PREFERENCE_SCHEMA_VERSION ||
    !text(value.walletId) ||
    !text(value.networkGenesis) ||
    typeof value.targetLovelace !== 'string' ||
    !/^(?:0|[1-9][0-9]*)$/u.test(value.targetLovelace) ||
    !Array.isArray(value.preferredInputs) ||
    !value.preferredInputs.length ||
    !uint(value.generation)
  )
    throw new Error('Invalid collateral preference');
  const preferredInputs = value.preferredInputs.map((input) => {
    if (
      !object(input) ||
      !ownKeysAre(input, ['transactionId', 'index']) ||
      !transactionId(input.transactionId) ||
      !uint(input.index) ||
      input.index > 0xffff
    )
      throw new Error('Invalid collateral preference');
    return Object.freeze({
      transactionId: input.transactionId,
      index: Number(input.index),
    });
  });
  const identities = preferredInputs.map(
    ({ transactionId: id, index }) => `${id}:${index}`
  );
  if (new Set(identities).size !== identities.length)
    throw new Error('Invalid collateral preference');
  return Object.freeze({
    schemaVersion: COLLATERAL_PREFERENCE_SCHEMA_VERSION,
    walletId: value.walletId,
    networkGenesis: value.networkGenesis,
    targetLovelace: value.targetLovelace,
    preferredInputs: Object.freeze(preferredInputs),
    generation: Number(value.generation),
  });
};

const sameIdentity = (
  record: CollateralPreferenceRecord,
  walletId: string,
  networkGenesis: string
) => record.walletId === walletId && record.networkGenesis === networkGenesis;

export class CollateralPreferenceStore {
  private records: readonly CollateralPreferenceRecord[] = [];
  private corrupt = false;

  public constructor(private readonly filePath: string) {
    this.load();
  }

  public get isCorrupt(): boolean {
    return this.corrupt;
  }

  public get(
    walletId: string,
    networkGenesis: string
  ): CollateralPreferenceRecord | undefined {
    if (this.corrupt) return undefined;
    return this.records.find((record) =>
      sameIdentity(record, walletId, networkGenesis)
    );
  }

  public put(
    value: Omit<CollateralPreferenceRecord, 'schemaVersion'>
  ): CollateralPreferenceRecord {
    if (this.corrupt)
      throw new Error('Collateral preference repository requires repair');
    const record = parseRecord({
      ...value,
      schemaVersion: COLLATERAL_PREFERENCE_SCHEMA_VERSION,
    });
    this.save([
      ...this.records.filter(
        (current) =>
          !sameIdentity(current, record.walletId, record.networkGenesis)
      ),
      record,
    ]);
    return record;
  }

  public clear(walletId: string, networkGenesis: string): void {
    this.replace(
      this.records.filter(
        (record) => !sameIdentity(record, walletId, networkGenesis)
      )
    );
  }

  public repair(): void {
    this.save([]);
  }

  private load(): void {
    if (!fs.existsSync(this.filePath)) return;
    try {
      const stored: unknown = JSON.parse(
        fs.readFileSync(this.filePath, 'utf8')
      );
      if (
        !object(stored) ||
        !ownKeysAre(stored, ['schemaVersion', 'preferences']) ||
        stored.schemaVersion !== COLLATERAL_PREFERENCE_SCHEMA_VERSION ||
        !Array.isArray(stored.preferences)
      )
        throw new Error('Invalid collateral preference repository');
      const records = stored.preferences.map(parseRecord);
      const identities = records.map(
        ({ walletId, networkGenesis }) => `${walletId}\0${networkGenesis}`
      );
      if (new Set(identities).size !== identities.length)
        throw new Error('Duplicate collateral preference');
      this.records = Object.freeze(records);
    } catch {
      this.records = [];
      this.corrupt = true;
    }
  }

  private replace(records: readonly CollateralPreferenceRecord[]): void {
    if (
      records.length !== this.records.length ||
      records.some((record, index) => record !== this.records[index])
    )
      this.save(records);
  }

  private save(records: readonly CollateralPreferenceRecord[]): void {
    fs.mkdirSync(path.dirname(this.filePath), { recursive: true, mode: 0o700 });
    const temporaryPath = `${this.filePath}.tmp`;
    const descriptor = fs.openSync(temporaryPath, 'w', 0o600);
    try {
      fs.writeFileSync(
        descriptor,
        `${JSON.stringify({
          schemaVersion: COLLATERAL_PREFERENCE_SCHEMA_VERSION,
          preferences: records,
        })}\n`
      );
      fs.fsyncSync(descriptor);
    } finally {
      fs.closeSync(descriptor);
    }
    fs.renameSync(temporaryPath, this.filePath);
    fs.chmodSync(this.filePath, 0o600);
    this.records = Object.freeze([...records]);
    this.corrupt = false;
  }
}
