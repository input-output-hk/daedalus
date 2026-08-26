import fs from 'fs';
import path from 'path';
import {
  DappGrant,
  DappGrantLaunch,
  DappScope,
} from '../../common/types/dapp.types';
import { canonicalizeDappOrigin } from '../dapp/urlPolicy';

export const DAPP_GRANT_SCHEMA_VERSION = 1;

const PERSISTED_SCOPES = new Set<DappScope>([
  'connection',
  'read',
  'governance-key-disclosure',
  'account-public-key-disclosure',
]);
const EXTENSIONS = new Set([95, 103, 104, 142]);
const ownKeysAre = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const isObject = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);
const isText = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
const unique = <T>(values: readonly T[]) =>
  new Set(values).size === values.length;

const parseLaunch = (value: unknown): DappGrantLaunch => {
  if (!isObject(value) || !isText(value.kind)) throw new Error('Invalid grant');
  if (value.kind === 'diagnostics' && ownKeysAre(value, ['kind']))
    return Object.freeze({ kind: 'diagnostics' });
  if (
    value.kind === 'catalog' &&
    ownKeysAre(value, ['kind', 'catalogEntryId', 'catalogEntryIdentity']) &&
    isText(value.catalogEntryId) &&
    isText(value.catalogEntryIdentity)
  )
    return Object.freeze({
      kind: 'catalog',
      catalogEntryId: value.catalogEntryId,
      catalogEntryIdentity: value.catalogEntryIdentity,
    });
  throw new Error('Invalid grant');
};

const parseGrant = (value: unknown, canonical = true): DappGrant => {
  if (
    !isObject(value) ||
    !ownKeysAre(value, [
      'schemaVersion',
      'origin',
      'walletId',
      'networkGenesis',
      'networkMagic',
      'readScopes',
      'enabledExtensionScopes',
      'launch',
      'grantedAt',
    ]) ||
    value.schemaVersion !== DAPP_GRANT_SCHEMA_VERSION ||
    !isText(value.origin) ||
    !isText(value.walletId) ||
    !isText(value.networkGenesis) ||
    !Number.isSafeInteger(value.networkMagic) ||
    Number(value.networkMagic) < 0 ||
    Number(value.networkMagic) > 0xffffffff ||
    !Array.isArray(value.readScopes) ||
    !unique(value.readScopes) ||
    !value.readScopes.every((scope) => PERSISTED_SCOPES.has(scope)) ||
    !Array.isArray(value.enabledExtensionScopes) ||
    !unique(value.enabledExtensionScopes) ||
    !value.enabledExtensionScopes.every((cip) => EXTENSIONS.has(cip)) ||
    !isText(value.grantedAt) ||
    new Date(value.grantedAt).toISOString() !== value.grantedAt
  )
    throw new Error('Invalid grant');

  const origin = canonicalizeDappOrigin(value.origin);
  if (canonical && origin !== value.origin) throw new Error('Invalid grant');
  return Object.freeze({
    schemaVersion: DAPP_GRANT_SCHEMA_VERSION,
    origin,
    walletId: value.walletId,
    networkGenesis: value.networkGenesis,
    networkMagic: Number(value.networkMagic),
    readScopes: Object.freeze([...value.readScopes]) as readonly DappScope[],
    enabledExtensionScopes: Object.freeze([
      ...value.enabledExtensionScopes,
    ]) as readonly number[],
    launch: parseLaunch(value.launch),
    grantedAt: value.grantedAt,
  });
};

export type GrantIdentity = Readonly<{
  origin: string;
  walletId: string;
  networkGenesis: string;
  launch: DappGrantLaunch;
}>;

export type GrantRequirement = GrantIdentity &
  Readonly<{
    scopes?: readonly DappScope[];
    extensions?: readonly number[];
  }>;

const sameLaunch = (left: DappGrantLaunch, right: DappGrantLaunch) =>
  left.kind === right.kind &&
  (left.kind === 'diagnostics' ||
    (right.kind === 'catalog' &&
      left.catalogEntryId === right.catalogEntryId &&
      left.catalogEntryIdentity === right.catalogEntryIdentity));

const sameIdentity = (grant: DappGrant, identity: GrantIdentity) =>
  grant.origin === canonicalizeDappOrigin(identity.origin) &&
  grant.walletId === identity.walletId &&
  grant.networkGenesis === identity.networkGenesis &&
  sameLaunch(grant.launch, identity.launch);

export class GrantRepository {
  private grants: readonly DappGrant[] = [];
  private corrupt = false;
  private readonly filePath: string;

  constructor(filePath: string) {
    this.filePath = filePath;
    this.load();
  }

  get isCorrupt(): boolean {
    return this.corrupt;
  }

  list(): readonly DappGrant[] {
    return this.grants;
  }

  find(requirement: GrantRequirement): DappGrant | undefined {
    if (this.corrupt) return undefined;
    try {
      return this.grants.find(
        (grant) =>
          sameIdentity(grant, requirement) &&
          (requirement.scopes ?? []).every((scope) =>
            grant.readScopes.includes(scope)
          ) &&
          (requirement.extensions ?? []).every((cip) =>
            grant.enabledExtensionScopes.includes(cip)
          )
      );
    } catch {
      return undefined;
    }
  }

  put(value: Omit<DappGrant, 'schemaVersion'>): DappGrant {
    if (this.corrupt) throw new Error('Grant repository requires repair');
    const grant = parseGrant(
      { ...value, schemaVersion: DAPP_GRANT_SCHEMA_VERSION },
      false
    );
    const next = [
      ...this.grants.filter((current) => !sameIdentity(current, grant)),
      grant,
    ];
    this.save(next);
    return grant;
  }

  forget(identity: GrantIdentity): void {
    this.update((grant) => !sameIdentity(grant, identity));
  }

  removeWallet(walletId: string): void {
    this.update((grant) => grant.walletId !== walletId);
  }

  pruneCatalog(currentEntries: ReadonlyMap<string, string>): void {
    this.update(
      (grant) =>
        grant.launch.kind === 'diagnostics' ||
        currentEntries.get(grant.launch.catalogEntryId) ===
          grant.launch.catalogEntryIdentity
    );
  }

  revokeScopes(identity: GrantIdentity, scopes: readonly DappScope[]): void {
    const revoked = new Set(scopes);
    this.replace(
      this.grants.map((grant) =>
        sameIdentity(grant, identity)
          ? Object.freeze({
              ...grant,
              readScopes: Object.freeze(
                grant.readScopes.filter((scope) => !revoked.has(scope))
              ),
            })
          : grant
      )
    );
  }

  repair(): void {
    this.save([]);
  }

  private load(): void {
    if (!fs.existsSync(this.filePath)) return;
    try {
      const stored: unknown = JSON.parse(
        fs.readFileSync(this.filePath, 'utf8')
      );
      if (
        !isObject(stored) ||
        !ownKeysAre(stored, ['schemaVersion', 'grants']) ||
        stored.schemaVersion !== DAPP_GRANT_SCHEMA_VERSION ||
        !Array.isArray(stored.grants)
      )
        throw new Error('Invalid grant repository');
      const grants = stored.grants.map((grant) => parseGrant(grant));
      if (
        grants.some((grant, index) =>
          grants.slice(index + 1).some((other) => sameIdentity(other, grant))
        )
      )
        throw new Error('Duplicate grant');
      this.grants = Object.freeze(grants);
    } catch {
      this.grants = [];
      this.corrupt = true;
    }
  }

  // eslint-disable-next-line no-unused-vars
  private update(keep: (grant: DappGrant) => boolean): void {
    this.replace(this.grants.filter(keep));
  }

  private replace(grants: readonly DappGrant[]): void {
    if (
      grants.length !== this.grants.length ||
      grants.some((grant, index) => grant !== this.grants[index])
    )
      this.save(grants);
  }

  private save(grants: readonly DappGrant[]): void {
    fs.mkdirSync(path.dirname(this.filePath), { recursive: true, mode: 0o700 });
    const temporaryPath = `${this.filePath}.tmp`;
    const descriptor = fs.openSync(temporaryPath, 'w', 0o600);
    try {
      fs.writeFileSync(
        descriptor,
        `${JSON.stringify({
          schemaVersion: DAPP_GRANT_SCHEMA_VERSION,
          grants,
        })}\n`
      );
      fs.fsyncSync(descriptor);
    } finally {
      fs.closeSync(descriptor);
    }
    fs.renameSync(temporaryPath, this.filePath);
    fs.chmodSync(this.filePath, 0o600);
    this.grants = Object.freeze([...grants]);
    this.corrupt = false;
  }
}
