import fs from 'fs';
import os from 'os';
import path from 'path';
import type { DappCapability, DappGrant } from '../../common/types/dapp.types';
import {
  DAPP_POLICY_REVISION,
  DappLaunchPolicy,
} from '../dapp/DappLaunchPolicy';
import { CapabilityService } from './CapabilityService';
import {
  DAPP_GRANT_SCHEMA_VERSION,
  GrantIdentity,
  GrantRepository,
} from './GrantRepository';
import { ExtensionRegistry } from './ExtensionRegistry';
import { Negotiator } from './Negotiator';
import { CapabilityRequirement, SessionStore } from './SessionStore';

const directory = () => fs.mkdtempSync(path.join(os.tmpdir(), 'dapp-grants-'));
const grant = (
  overrides: Partial<Omit<DappGrant, 'schemaVersion'>> = {}
): Omit<DappGrant, 'schemaVersion'> => ({
  origin: 'https://example.com',
  walletId: 'wallet-1',
  networkGenesis: 'genesis-1',
  networkMagic: 1,
  readScopes: ['connection', 'read'],
  enabledExtensionScopes: [95, 103],
  launch: {
    kind: 'catalog',
    catalogEntryId: 'dex',
    catalogEntryIdentity: 'dex-v1',
  },
  grantedAt: '2026-08-26T00:00:00.000Z',
  ...overrides,
});
const identity = (value: Omit<DappGrant, 'schemaVersion'>): GrantIdentity => ({
  origin: value.origin,
  walletId: value.walletId,
  networkGenesis: value.networkGenesis,
  launch: value.launch,
});
const capability = (
  overrides: Partial<DappCapability> = {}
): DappCapability => ({
  guestWebContentsId: 7,
  documentGeneration: 2,
  dappId: 'dex',
  origin: 'https://example.com',
  connectionId: 'connection-1',
  walletId: 'wallet-1',
  routeEpoch: 3,
  networkId: 0,
  networkMagic: 1,
  networkGenesis: 'genesis-1',
  enabledExtensions: [95],
  grantedScopes: ['connection', 'read'],
  ...overrides,
});
const capabilityRequirement = (
  value: DappCapability
): CapabilityRequirement => ({
  guestWebContentsId: value.guestWebContentsId,
  documentGeneration: value.documentGeneration,
  origin: value.origin,
  connectionId: value.connectionId,
  walletId: value.walletId,
  routeEpoch: value.routeEpoch,
  networkId: value.networkId,
  networkMagic: value.networkMagic,
  networkGenesis: value.networkGenesis,
  launch:
    value.dappId === undefined
      ? { kind: 'diagnostics' }
      : {
          kind: 'catalog',
          catalogEntryId: value.dappId,
          catalogEntryIdentity: 'current',
        },
  requiredExtensions: [95],
  requiredScopes: ['read'],
});

describe('main-owned dApp authority stores', () => {
  const directories: string[] = [];
  const repository = () => {
    const root = directory();
    directories.push(root);
    return new GrantRepository(path.join(root, 'grants.json'));
  };

  afterEach(() => {
    directories
      .splice(0)
      .forEach((root) => fs.rmSync(root, { recursive: true, force: true }));
  });

  it('persists only canonical reusable read authority atomically', () => {
    const root = directory();
    directories.push(root);
    const file = path.join(root, 'grants.json');
    const grants = new GrantRepository(file);
    const stored = grants.put(
      grant({ origin: 'https://EXAMPLE.com:443', readScopes: ['connection'] })
    );

    expect(stored.origin).toBe('https://example.com');
    expect(fs.statSync(file).mode & 0o777).toBe(0o600);
    expect(fs.existsSync(`${file}.tmp`)).toBe(false);
    expect(new GrantRepository(file).list()).toEqual([stored]);
    expect(
      grants.find({ ...identity(stored), scopes: ['connection'] })
    ).toEqual(stored);
    expect(
      grants.find({ ...identity(stored), scopes: ['transaction-signing'] })
    ).toBeUndefined();
    expect(() =>
      grants.put(grant({ readScopes: ['transaction-submission'] }))
    ).toThrow('Invalid grant');
    expect(() =>
      grants.put(grant({ origin: 'https://user@example.com' }))
    ).toThrow('Invalid dApp URL');
    expect(() =>
      grants.put(grant({ origin: 'https://example.com/path' }))
    ).toThrow('Invalid dApp origin');
  });

  it('keeps development loopback authority policy-bound and origin-only', () => {
    const root = directory();
    directories.push(root);
    const file = path.join(root, 'grants.json');
    const development = { allowHttpLoopback: true };
    const grants = new GrantRepository(file, development);
    const stored = grants.put(
      grant({
        origin: 'http://LOCALHOST:3000',
        launch: { kind: 'diagnostics' },
      })
    );

    expect(stored.origin).toBe('http://localhost:3000');
    expect(fs.readFileSync(file, 'utf8')).not.toContain('/private');
    expect(new GrantRepository(file).isCorrupt).toBe(true);
    expect(new GrantRepository(file, development).list()).toEqual([stored]);

    const sessions = new SessionStore(development);
    const live = sessions.create(
      capability({
        dappId: undefined,
        origin: 'http://LOCALHOST:3000',
      })
    );
    expect(
      sessions.get({
        ...capabilityRequirement(live),
        launch: { kind: 'diagnostics' },
      })
    ).toEqual(live);
  });

  it('fails closed on corruption until explicit repair', () => {
    const root = directory();
    directories.push(root);
    const file = path.join(root, 'grants.json');
    fs.writeFileSync(file, '{bad json', { mode: 0o600 });
    const grants = new GrantRepository(file);

    expect(grants.isCorrupt).toBe(true);
    expect(grants.list()).toEqual([]);
    expect(() => grants.put(grant())).toThrow('requires repair');
    grants.repair();
    expect(grants.isCorrupt).toBe(false);
    expect(JSON.parse(fs.readFileSync(file, 'utf8')).schemaVersion).toBe(
      DAPP_GRANT_SCHEMA_VERSION
    );
  });

  it('invalidates catalog, wallet, forgotten, and expanded-scope authority', () => {
    const grants = repository();
    const catalog = grants.put(grant());
    const diagnostics = grants.put(
      grant({
        origin: 'https://diagnostics.example',
        launch: { kind: 'diagnostics' },
      })
    );

    expect(
      grants.find({
        ...identity(catalog),
        scopes: ['connection', 'governance-key-disclosure'],
      })
    ).toBeUndefined();
    grants.pruneCatalog(new Map([['dex', 'changed-v2']]));
    expect(grants.find(identity(catalog))).toBeUndefined();
    expect(grants.find(identity(diagnostics))).toEqual(diagnostics);

    grants.revokeScopes(identity(diagnostics), ['read']);
    expect(
      grants.find({ ...identity(diagnostics), scopes: ['read'] })
    ).toBeUndefined();
    grants.forget(identity(diagnostics));
    expect(grants.list()).toEqual([]);
    const elevated = grants.put(
      grant({
        readScopes: ['connection', 'read', 'governance-key-disclosure'],
        enabledExtensionScopes: [95],
      })
    );
    expect(
      grants.find({
        ...identity(elevated),
        scopes: ['governance-key-disclosure'],
        extensions: [95],
      })
    ).toEqual(elevated);
    grants.revokeScopes(identity(elevated), ['governance-key-disclosure']);
    expect(
      grants.find({
        ...identity(elevated),
        scopes: ['governance-key-disclosure'],
        extensions: [95],
      })
    ).toBeUndefined();
    expect(
      grants.find({
        ...identity(elevated),
        scopes: ['connection', 'read'],
      })
    ).toBeDefined();
    grants.forget(identity(elevated));

    const walletGrant = grants.put(grant());
    grants.removeWallet(walletGrant.walletId);
    expect(grants.list()).toEqual([]);
  });

  it('keeps capabilities ephemeral and rejects every stale identity', async () => {
    const sessions = new SessionStore();
    const live = sessions.create(
      capability({ origin: 'https://EXAMPLE.com:443' })
    );
    const exact = capabilityRequirement(live);

    expect(sessions.get(exact)).toEqual(live);
    expect(
      sessions.get({
        ...exact,
        documentGeneration: exact.documentGeneration + 1,
      })
    ).toBeUndefined();
    expect(
      sessions.get({ ...exact, routeEpoch: exact.routeEpoch + 1 })
    ).toBeUndefined();
    expect(
      sessions.get({ ...exact, networkGenesis: 'different-genesis' })
    ).toBeUndefined();
    expect(
      sessions.get({ ...exact, launch: { kind: 'diagnostics' } })
    ).toBeUndefined();
    expect(
      sessions.get({ ...exact, requiredExtensions: [142] })
    ).toBeUndefined();
    expect(
      sessions.get({ ...exact, requiredScopes: ['transaction-signing'] })
    ).toBeUndefined();

    const authorizedSubmission = Promise.resolve('submitted');
    sessions.revokeGrant(identity(grant()));
    expect(sessions.get(exact)).toBeUndefined();
    await expect(authorizedSubmission).resolves.toBe('submitted');

    const replacement = sessions.create(
      capability({ connectionId: 'replacement' })
    );
    sessions.revokeWallet(replacement.walletId);
    expect(sessions.get(capabilityRequirement(replacement))).toBeUndefined();

    const network = sessions.create(capability({ connectionId: 'network' }));
    sessions.revokeNetwork(network.networkGenesis);
    expect(sessions.get(capabilityRequirement(network))).toBeUndefined();

    const route = sessions.create(capability({ connectionId: 'route' }));
    sessions.revokeRoute(route.walletId, route.routeEpoch);
    expect(sessions.get(capabilityRequirement(route))).toBeUndefined();
  });

  it('omits launcher-disabled proposed namespaces before session creation', () => {
    const registry = new ExtensionRegistry();
    const negotiator = new Negotiator(
      registry,
      new CapabilityService(registry)
    );
    const context = {
      walletKind: 'shelley-software' as const,
      backendApiVersion: 1,
      backendExtensions: [95, 103, 104],
      networkSupported: true,
      policy: new DappLaunchPolicy({
        revision: DAPP_POLICY_REVISION,
        globalEnabled: true,
        preferredCatalogEnabled: true,
        diagnosticsEnabled: true,
        cip104Revision: 0,
        cip142Revision: 0,
      }),
    };

    expect(
      negotiator.negotiate(
        { extensions: [{ cip: 104 }, { cip: 142 }] },
        context
      ).enabledExtensions
    ).toEqual([]);
  });
});
