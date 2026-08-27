import fs from 'fs';
import os from 'os';
import path from 'path';
import type { DappCapability, DappGrant } from '../../common/types/dapp.types';
import { DappConnectionService } from './DappConnectionService';
import type { ConsentCoordinator } from './ConsentCoordinator';
import { GrantRepository } from './GrantRepository';
import { SessionStore } from './SessionStore';

const grant = (
  overrides: Partial<Omit<DappGrant, 'schemaVersion'>> = {}
): Omit<DappGrant, 'schemaVersion'> => ({
  origin: 'https://example.com',
  walletId: 'wallet-1',
  networkGenesis: 'genesis-1',
  networkMagic: 1,
  readScopes: ['connection', 'read', 'governance-key-disclosure'],
  enabledExtensionScopes: [95],
  launch: {
    kind: 'catalog',
    catalogEntryId: 'dex',
    catalogEntryIdentity: 'dex-v1',
  },
  grantedAt: '2026-08-27T00:00:00.000Z',
  ...overrides,
});
const identity = (value: DappGrant) => ({
  origin: value.origin,
  walletId: value.walletId,
  networkGenesis: value.networkGenesis,
  launch: value.launch,
});
const capability = (
  overrides: Partial<DappCapability> = {}
): DappCapability => ({
  guestWebContentsId: 1,
  documentGeneration: 1,
  dappId: 'dex',
  origin: 'https://example.com',
  connectionId: 'connection-1',
  walletId: 'wallet-1',
  routeEpoch: 1,
  networkId: 0,
  networkMagic: 1,
  networkGenesis: 'genesis-1',
  enabledExtensions: [95],
  grantedScopes: ['connection', 'read', 'governance-key-disclosure'],
  ...overrides,
});

describe('DappConnectionService', () => {
  let root: string;
  let grants: GrantRepository;
  let sessions: SessionStore;
  let cancel: jest.Mock;
  let service: DappConnectionService;

  beforeEach(() => {
    root = fs.mkdtempSync(path.join(os.tmpdir(), 'dapp-connections-'));
    grants = new GrantRepository(path.join(root, 'grants.json'));
    sessions = new SessionStore();
    cancel = jest.fn();
    service = new DappConnectionService(grants, sessions, ({
      cancel,
    } as unknown) as ConsentCoordinator);
  });

  afterEach(() => fs.rmSync(root, { recursive: true, force: true }));

  it('distinguishes disconnect, scope revocation, and forget', () => {
    const stored = grants.put(grant());
    sessions.create(capability());

    service.disconnect(identity(stored));
    expect(grants.find(identity(stored))).toEqual(stored);
    expect(sessions.currentForGuest(1)).toBeUndefined();

    sessions.create(capability({ connectionId: 'connection-2' }));
    service.revokeScope(identity(stored), 'governance-key-disclosure');
    expect(sessions.currentForGuest(1)).toBeUndefined();
    expect(
      grants.find({
        ...identity(stored),
        scopes: ['governance-key-disclosure'],
      })
    ).toBeUndefined();
    expect(
      grants.find({ ...identity(stored), scopes: ['read'] })
    ).toBeDefined();

    sessions.create(capability({ connectionId: 'connection-3' }));
    service.forget(identity(stored));
    expect(sessions.currentForGuest(1)).toBeUndefined();
    expect(grants.find(identity(stored))).toBeUndefined();
    expect(cancel).toHaveBeenCalledTimes(3);
  });

  it('prunes only absent wallets and revokes their live authority', () => {
    grants.put(grant());
    grants.put(
      grant({
        origin: 'https://other.example',
        walletId: 'wallet-2',
        launch: { kind: 'diagnostics' },
      })
    );
    sessions.create(capability());
    sessions.create(
      capability({
        guestWebContentsId: 2,
        connectionId: 'connection-2',
        dappId: undefined,
        origin: 'https://other.example',
        walletId: 'wallet-2',
      })
    );

    service.pruneWallets(['wallet-2']);

    expect(grants.list().map((item) => item.walletId)).toEqual(['wallet-2']);
    expect(sessions.currentForGuest(1)).toBeUndefined();
    expect(sessions.currentForGuest(2)).toBeDefined();
  });

  it('repairs corruption fail closed and revokes every session', () => {
    fs.writeFileSync(path.join(root, 'grants.json'), '{bad json');
    grants = new GrantRepository(path.join(root, 'grants.json'));
    sessions.create(capability());
    service = new DappConnectionService(grants, sessions, ({
      cancel,
    } as unknown) as ConsentCoordinator);

    expect(service.snapshot().corrupt).toBe(true);
    expect(service.repair()).toEqual({ corrupt: false, grants: [] });
    expect(sessions.currentForGuest(1)).toBeUndefined();
  });
});
