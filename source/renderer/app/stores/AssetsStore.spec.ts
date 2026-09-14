/**
 * `AssetsStore` reading the cache rather than the endpoint.
 *
 * The store is built directly and `initialize()` is never called, so no IPC
 * channel is registered; the renderer client is mocked and driven by hand.
 * Strict mode is configured here as the application configures it, so a
 * mutation outside an action fails a case rather than passing quietly.
 */
import { autorun, configure } from 'mobx';
import AssetsStore from './AssetsStore';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import { noopAnalyticsTracker } from '../analytics';
import { assetFingerprint } from '../utils/assetFingerprint';
import { getAssetTokenFromToken, searchAssets } from '../utils/assets';

jest.mock('../ipc/assetMetadataChannel', () => ({
  requestAssetMetadata: jest.fn(),
  onAssetMetadataUpdate: jest.fn(),
}));

const { requestAssetMetadata, onAssetMetadataUpdate } = jest.requireMock(
  '../ipc/assetMetadataChannel'
);

configure({ enforceActions: 'observed' });

const POLICY = 'c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9';
const ASSET_NAME = '42544544';
const SUBJECT = `${POLICY}${ASSET_NAME}`;
const OTHER_POLICY = 'a'.repeat(56);
const OTHER_SUBJECT = `${OTHER_POLICY}beef`;

const entry = (overrides: Record<string, any> = {}) => ({
  subject: SUBJECT,
  policyId: POLICY,
  assetName: ASSET_NAME,
  ticker: 'BTED',
  name: 'Bit-ED',
  decimals: 6,
  verified: true,
  source: 'registry',
  hasImage: false,
  metadata: { url: 'https://bit-ed.org/' },
  ...overrides,
});

const makeStore = (storedDecimals: Record<string, any> = {}) => {
  const localStorage = {
    getWalletTokenFavorites: jest.fn().mockResolvedValue({}),
    getAssetsLocalData: jest.fn().mockResolvedValue(storedDecimals),
    setAssetLocalData: jest.fn().mockResolvedValue(undefined),
    toggleWalletTokenFavorite: jest.fn().mockResolvedValue(undefined),
  };
  const api = { ada: {}, localStorage } as unknown as Api;
  const actions = {} as unknown as ActionsMap;
  const store = new AssetsStore(api, actions, noopAnalyticsTracker);
  store.configure({
    wallets: { active: null },
    transactions: { all: [] },
    networkStatus: { isConnected: false },
  } as any);
  return { store, localStorage };
};

const withHoldings = (store: AssetsStore, subjects: Array<string>) => {
  (store as any).stores.wallets.active = {
    id: 'wallet-1',
    assets: {
      total: subjects.map((subject) => ({
        policyId: subject.slice(0, 56),
        assetName: subject.slice(56),
      })),
    },
  };
};

const tokenFor = (subject: string) => ({
  policyId: subject.slice(0, 56),
  assetName: subject.slice(56),
  uniqueId: subject,
  quantity: { isZero: () => false } as any,
});

describe('AssetsStore', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    requestAssetMetadata.mockResolvedValue({
      requestId: 'r',
      entries: [],
      unresolved: [],
    });
  });

  describe('getAsset', () => {
    it('returns a cached row without asking for it again', () => {
      const { store } = makeStore();
      (store as any)._onMetadataResolved({ entries: [entry()] });
      const asset = store.getAsset(POLICY, ASSET_NAME);
      expect(asset.uniqueId).toBe(SUBJECT);
      expect(asset.metadata.ticker).toBe('BTED');
      expect(asset.metadata.name).toBe('Bit-ED');
      expect(asset.recommendedDecimals).toBe(6);
      expect(asset.fingerprint).toBe(assetFingerprint(POLICY, ASSET_NAME));
      expect(requestAssetMetadata).not.toHaveBeenCalled();
    });

    it('returns identity and a locally computed fingerprint for an unresolved subject', () => {
      const { store } = makeStore();
      const asset = store.getAsset(OTHER_POLICY, 'beef');
      expect(asset.uniqueId).toBe(OTHER_SUBJECT);
      expect(asset.policyId).toBe(OTHER_POLICY);
      expect(asset.fingerprint).toBe(assetFingerprint(OTHER_POLICY, 'beef'));
      expect(asset.metadata).toBeNull();
      expect(asset.recommendedDecimals).toBeNull();
      expect(requestAssetMetadata).not.toHaveBeenCalled();
    });

    it('returns the same row for an unresolved subject rather than a new one each time', () => {
      const { store } = makeStore();
      expect(store.getAsset(OTHER_POLICY, 'beef')).toBe(
        store.getAsset(OTHER_POLICY, 'beef')
      );
    });

    it('returns nothing when the identity cannot have a fingerprint', () => {
      const { store } = makeStore();
      expect(store.getAsset('too-short', 'beef')).toBeNull();
      expect(store.getAsset('zz'.repeat(28), 'beef')).toBeNull();
      expect(store.getAsset(OTHER_POLICY, 'ab'.repeat(33))).toBeNull();
    });

    it('prefers the registry name over nothing and keeps an empty name falsy', () => {
      const { store } = makeStore();
      (store as any)._onMetadataResolved({
        entries: [
          entry({ ticker: null, name: null, decimals: null, metadata: null }),
        ],
      });
      const asset = store.getAsset(POLICY, ASSET_NAME);
      expect(asset.metadata).toBeNull();
      expect(asset.fingerprint).toBe(assetFingerprint(POLICY, ASSET_NAME));
    });
  });

  describe('details', () => {
    it('is keyed by subject', () => {
      const { store } = makeStore();
      (store as any)._onMetadataResolved({ entries: [entry()] });
      expect(Object.keys(store.details)).toEqual([SUBJECT]);
      expect(store.details[SUBJECT].metadata.ticker).toBe('BTED');
    });
  });

  describe('the update channel', () => {
    it('makes a resolved row visible to an observer without another request', () => {
      const { store } = makeStore();
      const seen: Array<any> = [];
      const dispose = autorun(() => {
        const asset = store.getAsset(POLICY, ASSET_NAME);
        seen.push(asset ? asset.metadata?.ticker : undefined);
      });
      expect(seen).toEqual([undefined]);

      (store as any)._onMetadataResolved({ entries: [entry()] });

      expect(seen).toEqual([undefined, 'BTED']);
      expect(requestAssetMetadata).not.toHaveBeenCalled();
      dispose();
    });

    it('subscribes through the update channel during setup', () => {
      const { store } = makeStore();
      (store as any).actions = {
        assets: {
          setEditedAsset: { listen: jest.fn() },
          onAssetSettingsSubmit: { listen: jest.fn() },
          unsetEditedAsset: { listen: jest.fn() },
          onOpenAssetSend: { listen: jest.fn() },
          onCopyAssetParam: { listen: jest.fn() },
          onToggleFavorite: { listen: jest.fn() },
        },
        wallets: {
          setActiveAsset: { listen: jest.fn() },
          unsetActiveAsset: { listen: jest.fn() },
        },
      };
      store.setup();
      expect(onAssetMetadataUpdate).toHaveBeenCalledTimes(1);
    });
  });

  describe('the subject list', () => {
    it('merges holdings and transaction assets, deduplicated, and asks once', () => {
      const { store } = makeStore();
      withHoldings(store, [SUBJECT, OTHER_SUBJECT]);
      (store as any).stores.transactions.all = [
        { assets: [{ policyId: POLICY, assetName: ASSET_NAME }] },
        { assets: [{ policyId: 'b'.repeat(56), assetName: '01' }] },
      ];

      (store as any)._resolveRenderedSubjects();

      expect(requestAssetMetadata).toHaveBeenCalledTimes(1);
      expect(requestAssetMetadata.mock.calls[0][0]).toEqual([
        SUBJECT,
        OTHER_SUBJECT,
        `${'b'.repeat(56)}01`,
      ]);
    });

    it('does not ask again for a subject it has already asked for', () => {
      const { store } = makeStore();
      withHoldings(store, [SUBJECT]);

      (store as any)._resolveRenderedSubjects();
      (store as any)._resolveRenderedSubjects();

      expect(requestAssetMetadata).toHaveBeenCalledTimes(1);
    });

    it('asks for a newly held subject on its own', () => {
      const { store } = makeStore();
      withHoldings(store, [SUBJECT]);
      (store as any)._resolveRenderedSubjects();

      withHoldings(store, [SUBJECT, OTHER_SUBJECT]);
      (store as any)._resolveRenderedSubjects();

      expect(requestAssetMetadata).toHaveBeenCalledTimes(2);
      expect(requestAssetMetadata.mock.calls[1][0]).toEqual([OTHER_SUBJECT]);
    });

    it('merges the entries the response carries', async () => {
      const { store } = makeStore();
      requestAssetMetadata.mockResolvedValue({
        requestId: 'r',
        entries: [entry()],
        unresolved: [{ subject: OTHER_SUBJECT, state: 'pending' }],
      });
      withHoldings(store, [SUBJECT, OTHER_SUBJECT]);

      await (store as any)._requestMetadata([SUBJECT, OTHER_SUBJECT]);

      expect(store.getAsset(POLICY, ASSET_NAME).metadata.ticker).toBe('BTED');
      expect(store.getAsset(OTHER_POLICY, 'beef').metadata).toBeNull();
    });
  });

  describe('the per-token decimal setting', () => {
    it('is read from browser storage at startup', async () => {
      const { store } = makeStore({ [SUBJECT]: { decimals: 4 } });
      await (store as any)._setUpLocalDecimals();
      expect(store.getAsset(POLICY, ASSET_NAME).decimals).toBe(4);
    });

    it('is written to browser storage and read back through the store', async () => {
      const { store, localStorage } = makeStore();
      (store as any)._onMetadataResolved({ entries: [entry()] });

      await (store as any)._onAssetSettingsSubmit({
        asset: { policyId: POLICY, assetName: ASSET_NAME },
        decimals: 2,
      });

      expect(localStorage.setAssetLocalData).toHaveBeenCalledWith(
        POLICY,
        ASSET_NAME,
        { decimals: 2 }
      );
      const asset = store.getAsset(POLICY, ASSET_NAME);
      expect(asset.decimals).toBe(2);
      // The registry's value is still reported separately, which is what the
      // disagreement warning compares against.
      expect(asset.recommendedDecimals).toBe(6);
    });

    it('applies to a subject the cache has no row for', async () => {
      const { store } = makeStore();
      await (store as any)._onAssetSettingsSubmit({
        asset: { policyId: OTHER_POLICY, assetName: 'beef' },
        decimals: 3,
      });
      const asset = store.getAsset(OTHER_POLICY, 'beef');
      expect(asset.decimals).toBe(3);
      expect(asset.fingerprint).toBe(assetFingerprint(OTHER_POLICY, 'beef'));
    });
  });

  describe('setup', () => {
    const actionsFor = () => ({
      assets: {
        setEditedAsset: { listen: jest.fn() },
        onAssetSettingsSubmit: { listen: jest.fn() },
        unsetEditedAsset: { listen: jest.fn() },
        onOpenAssetSend: { listen: jest.fn() },
        onCopyAssetParam: { listen: jest.fn() },
        onToggleFavorite: { listen: jest.fn() },
      },
      wallets: {
        setActiveAsset: { listen: jest.fn() },
        unsetActiveAsset: { listen: jest.fn() },
      },
    });

    it('schedules nothing to repeat', () => {
      jest.useFakeTimers();
      const repeating = jest.spyOn(global, 'setInterval');
      const { store } = makeStore();
      (store as any).actions = actionsFor();

      store.setup();

      expect(repeating).not.toHaveBeenCalled();
      // Ten minutes of simulated time, against a poll that used to fire every
      // sixty seconds.
      jest.advanceTimersByTime(10 * 60 * 1000);
      expect(requestAssetMetadata).not.toHaveBeenCalled();
      repeating.mockRestore();
      jest.useRealTimers();
    });
  });

  describe('the merged row a surface renders', () => {
    it('carries identity and a fingerprint for a subject the cache has no row for', () => {
      const { store } = makeStore();
      const row = getAssetTokenFromToken(
        tokenFor(OTHER_SUBJECT) as any,
        store.getAsset
      );
      expect(row.uniqueId).toBe(OTHER_SUBJECT);
      expect(row.fingerprint).toBe(assetFingerprint(OTHER_POLICY, 'beef'));
      expect(row.metadata).toBeNull();
    });

    it('carries the cached metadata once the row resolves', () => {
      const { store } = makeStore();
      (store as any)._onMetadataResolved({ entries: [entry()] });
      const row = getAssetTokenFromToken(
        tokenFor(SUBJECT) as any,
        store.getAsset
      );
      expect(row.metadata.ticker).toBe('BTED');
      expect(row.fingerprint).toBe(assetFingerprint(POLICY, ASSET_NAME));
    });

    it('is still searchable by a name the registry published', () => {
      const { store } = makeStore();
      (store as any)._onMetadataResolved({
        entries: [entry({ name: 'Fundamental', ticker: null })],
      });
      const row = getAssetTokenFromToken(
        tokenFor(SUBJECT) as any,
        store.getAsset
      );
      expect(searchAssets('und', [row])).toHaveLength(1);
    });
  });
});
