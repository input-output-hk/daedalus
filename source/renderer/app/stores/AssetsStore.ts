import { observable, action, computed, runInAction } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Asset from '../domains/Asset';
import { ROUTES } from '../routes-config';
import { ellipsis } from '../utils/strings';
import { assetFingerprint } from '../utils/assetFingerprint';
import { getAssetTokenFromToken } from '../utils/assets';
import { resolveAssetDecimals } from '../utils/assetDecimals';
import {
  onAssetMetadataUpdate,
  requestAssetMetadata,
} from '../ipc/assetMetadataChannel';
import type { AssetMetadata, AssetToken } from '../api/assets/types';
import type { AssetMetadataEntry } from '../../../common/types/asset-metadata.types';
import { EventCategories } from '../analytics';

const subjectOf = (policyId: string, assetName: string): string =>
  `${policyId}${assetName}`;

/**
 * The cache stores a ticker, a name and the rest of the registry's properties
 * separately; the surfaces that render an asset expect one metadata object in
 * the shape the registry publishes. `name` and `description` are not optional
 * on that type, so a row the registry published nothing for carries the empty
 * string, which is falsy and therefore falls through the name resolution to the
 * decoded asset name rather than rendering as a published name.
 *
 * Returns null when the entry carries nothing at all, so that an entry with no
 * published properties is not mistaken for one with an empty name.
 */
const metadataOf = (entry: AssetMetadataEntry): AssetMetadata | null => {
  const extra = entry.metadata || {};
  const description =
    typeof extra.description === 'string' ? extra.description : '';
  const url = typeof extra.url === 'string' ? extra.url : undefined;
  const ticker = entry.ticker || undefined;
  const name = entry.name || '';
  const decimals = entry.decimals === null ? undefined : entry.decimals;

  if (!name && !ticker && !description && !url && decimals === undefined) {
    return null;
  }

  return {
    name,
    description,
    ticker,
    url,
    decimals,
  };
};

export default class AssetsStore extends Store {
  // REQUESTS
  @observable
  favoritesRequest: Request<Record<string, any>> = new Request(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    this.api.localStorage.getWalletTokenFavorites
  );
  @observable
  activeAsset: string | null | undefined = null;
  @observable
  _editedAsset: AssetToken | null | undefined = null;
  @observable
  insertingAssetUniqueId: string | null | undefined = null;
  @observable
  removingAssetUniqueId: string | null | undefined = null;

  // One row per subject the cache has resolved, filled by the request channel
  // and kept filled by the update channel.
  @observable
  _metadata: Map<string, AssetMetadataEntry> = new Map();

  // The per-token decimal setting, which is the user's and not the registry's.
  @observable
  _localDecimals: Map<string, number> = new Map();

  // A fingerprint is a pure function of identity, and a blake2b digest per
  // rendered row per paint is not free.
  _fingerprints: Map<string, string | null> = new Map();

  // Rows for subjects the cache has nothing for. Nothing about them can change
  // while they stay unresolved: the moment an entry or a setting arrives, the
  // map above answers instead.
  _unresolvedAssets: Map<string, Asset> = new Map();

  _requestedSubjects: Set<string> = new Set();

  setup() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AssetsS... Remove this comment to see the full error message
    const { assets: assetsActions, wallets: walletsActions } = this.actions;
    assetsActions.setEditedAsset.listen(this._onEditedAssetSet);
    assetsActions.onAssetSettingsRefresh.listen(this._onAssetSettingsRefresh);
    assetsActions.onAssetSettingsSubmit.listen(this._onAssetSettingsSubmit);
    assetsActions.unsetEditedAsset.listen(this._onEditedAssetUnset);
    assetsActions.onOpenAssetSend.listen(this._onOpenAssetSend);
    assetsActions.onCopyAssetParam.listen(this._onCopyAssetParam);
    assetsActions.onToggleFavorite.listen(this._onToggleFavorite);
    walletsActions.setActiveAsset.listen(this._setActiveAsset);
    walletsActions.unsetActiveAsset.listen(this._unsetActiveAsset);

    onAssetMetadataUpdate(this._onMetadataResolved);
    this.registerReactions([this._resolveRenderedSubjects]);

    this._setUpFavorites();
    this._setUpLocalDecimals();
  }

  // ==================== PUBLIC ==================
  @computed
  get details(): Record<string, Asset> {
    const details = {};
    const subjects = new Set([
      ...Array.from(this._metadata.keys()),
      ...Array.from(this._localDecimals.keys()),
    ]);
    subjects.forEach((subject) => {
      const asset = this._assetFor(subject);
      if (asset) details[subject] = asset;
    });
    return details;
  }

  /**
   * A token the wallet holds exists whether or not the cache has heard of it, so
   * a subject with no row still answers with its identity and its locally
   * computed fingerprint. Nothing comes back only when the identity cannot
   * produce one, which means it is not an asset this wallet could hold.
   */
  getAsset = (
    policyId: string,
    assetName: string
  ): Asset | null | undefined => {
    const subject = subjectOf(policyId, assetName);
    const resolved = this.details[subject];
    if (resolved) return resolved;
    return this._unresolvedAsset(subject, policyId, assetName);
  };

  /**
   * The token the settings dialog was opened on, overlaid with whatever the
   * cache knows now. Held as the row handed over and merged on read rather than
   * frozen at open time, so a row arriving on the update channel, including one
   * a manual refresh asked for, reaches a dialog that is still open.
   */
  @computed
  get editedAsset(): AssetToken | null | undefined {
    if (!this._editedAsset) return this._editedAsset;
    return getAssetTokenFromToken(this._editedAsset, this.getAsset);
  }

  @computed
  get favorites(): Record<string, any> {
    return this.favoritesRequest.result || {};
  }

  // =================== PRIVATE ==================
  _setUpFavorites = async () => {
    this.favoritesRequest.execute();
  };

  _setUpLocalDecimals = async () => {
    const stored = await this.api.localStorage.getAssetsLocalData();
    const decimals = Object.keys(stored || {}).reduce((found, subject) => {
      const value = stored[subject]?.decimals;
      if (typeof value === 'number') found.push([subject, value]);
      return found;
    }, []);
    if (decimals.length === 0) return;
    runInAction('AssetsStore::setUpLocalDecimals', () => {
      decimals.forEach(([subject, value]) => {
        this._localDecimals.set(subject, value);
      });
    });
  };

  /**
   * Asks the cache about everything on screen: what the wallet holds and what
   * the transactions being rendered name, which can include assets the wallet no
   * longer holds. A subject is asked for once. Whatever the cache cannot answer
   * now it schedules, and the rows arrive on the update channel.
   */
  _resolveRenderedSubjects = () => {
    const subjects = this._renderedSubjects();
    const wanted = subjects.filter(
      (subject) => !this._requestedSubjects.has(subject)
    );
    if (wanted.length === 0) return;
    wanted.forEach((subject) => this._requestedSubjects.add(subject));
    this._requestMetadata(wanted);
  };

  _renderedSubjects = (): Array<string> => {
    const subjects = new Set<string>();
    const holdings = get(this.stores, 'wallets.active.assets.total', []);
    holdings.forEach(({ policyId, assetName }) => {
      subjects.add(subjectOf(policyId, assetName));
    });
    const transactions = get(this.stores, 'transactions.all', []);
    transactions.forEach((transaction) => {
      (transaction.assets || []).forEach(({ policyId, assetName }) => {
        subjects.add(subjectOf(policyId, assetName));
      });
    });
    return Array.from(subjects);
  };

  _requestMetadata = async (
    subjects: Array<string>,
    options: { refresh?: boolean } = {}
  ) => {
    const response = await requestAssetMetadata(subjects, options);
    if (!response || response.entries.length === 0) return;
    runInAction('AssetsStore::mergeAssetMetadata', () => {
      response.entries.forEach((entry) => {
        this._metadata.set(entry.subject, entry);
      });
    });
  };

  @action
  _onMetadataResolved = ({
    entries,
  }: {
    entries: Array<AssetMetadataEntry>;
  }) => {
    (entries || []).forEach((entry) => {
      this._metadata.set(entry.subject, entry);
    });
  };

  _assetFor = (subject: string): Asset | null => {
    const entry = this._metadata.get(subject);
    const policyId = entry ? entry.policyId : subject.slice(0, 56);
    const assetName = entry ? entry.assetName : subject.slice(56);
    const fingerprint = this._fingerprintOf(policyId, assetName);
    if (fingerprint === null) return null;
    const metadata = entry ? metadataOf(entry) : null;
    const recommendedDecimals = entry ? entry.decimals : null;
    const recommendedDecimalsVerified = entry ? entry.verified : false;
    // The one place the user's setting and the registry's value meet. Every
    // surface reads the answer off `decimals` rather than deciding again.
    const { decimals } = resolveAssetDecimals({
      userDecimals: this._localDecimals.get(subject),
      registryDecimals: recommendedDecimals,
      registryDecimalsVerified: recommendedDecimalsVerified,
    });
    return new Asset({
      policyId,
      assetName,
      uniqueId: subject,
      fingerprint,
      metadata,
      decimals,
      recommendedDecimals,
      recommendedDecimalsVerified,
    });
  };

  _unresolvedAsset = (
    subject: string,
    policyId: string,
    assetName: string
  ): Asset | null => {
    const existing = this._unresolvedAssets.get(subject);
    if (existing) return existing;
    const fingerprint = this._fingerprintOf(policyId, assetName);
    if (fingerprint === null) return null;
    const asset = new Asset({
      policyId,
      assetName,
      uniqueId: subject,
      fingerprint,
      metadata: null,
      decimals: null,
      recommendedDecimals: null,
      recommendedDecimalsVerified: false,
    });
    this._unresolvedAssets.set(subject, asset);
    return asset;
  };

  /**
   * Null rather than a throw for an identity that cannot have a fingerprint: a
   * policy id of the wrong length or an asset name over the consensus limit is
   * not an asset any wallet holds, and a render is not the place to discover it.
   */
  _fingerprintOf = (policyId: string, assetName: string): string | null => {
    const subject = subjectOf(policyId, assetName);
    if (this._fingerprints.has(subject)) {
      return this._fingerprints.get(subject);
    }
    let fingerprint: string | null = null;
    try {
      fingerprint = assetFingerprint(policyId, assetName);
    } catch {
      fingerprint = null;
    }
    this._fingerprints.set(subject, fingerprint);
    return fingerprint;
  };

  @action
  _onEditedAssetSet = ({ asset }: { asset: AssetToken }) => {
    this._editedAsset = asset;
  };

  /**
   * One subject, the one the dialog is open on. The signature takes a single
   * asset rather than a list, so widening this into a bulk refresh is a change
   * to the signature rather than to an argument. Nothing here enumerates the
   * cache, and there is nothing to enumerate it with.
   */
  _onAssetSettingsRefresh = async ({ asset }: { asset: AssetToken }) => {
    const { policyId, assetName } = asset;
    await this._requestMetadata([subjectOf(policyId, assetName)], {
      refresh: true,
    });
  };
  @action
  _onAssetSettingsSubmit = async ({
    asset,
    decimals,
  }: {
    asset: AssetToken;
    decimals: number;
  }) => {
    this._editedAsset = null;
    const { policyId, assetName } = asset;
    this._localDecimals.set(subjectOf(policyId, assetName), decimals);

    await this.api.localStorage.setAssetLocalData(policyId, assetName, {
      decimals,
    });

    this.analytics.sendEvent(
      EventCategories.WALLETS,
      'Changed native token settings'
    );
  };
  @action
  _onEditedAssetUnset = () => {
    this._editedAsset = null;
  };
  @action
  _onOpenAssetSend = ({ uniqueId }: { uniqueId: string }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AssetsSt... Remove this comment to see the full error message
    const { stores, actions } = this;
    const { wallets } = stores;
    const { active } = wallets;

    if (active) {
      const { id } = active;
      const { wallets: walletActions, router } = actions;
      walletActions.setActiveAsset.trigger(uniqueId);
      router.goToRoute.trigger({
        route: ROUTES.WALLETS.PAGE,
        params: {
          id,
          page: 'send',
        },
      });
    }
  };
  @action
  _onCopyAssetParam = ({
    param,
    fullValue,
  }: {
    param: string;
    fullValue: string;
  }) => {
    const shortValue = ellipsis(fullValue, 15, 15);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AssetsS... Remove this comment to see the full error message
    this.actions.assets.copyAssetParamNotification.trigger({
      param,
      shortValue,
    });
  };
  @action
  _setActiveAsset = (uniqueId: string) => {
    this.activeAsset = uniqueId;
  };
  @action
  _unsetActiveAsset = () => {
    this.activeAsset = null;
  };
  @action
  _onToggleFavorite = async ({
    uniqueId,
    isFavorite,
  }: {
    uniqueId: string;
    isFavorite: boolean;
  }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    await this.api.localStorage.toggleWalletTokenFavorite(
      uniqueId,
      !isFavorite
    );
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.favoritesRequest.execute();

    this.analytics.sendEvent(
      EventCategories.WALLETS,
      `${!isFavorite ? 'Added token to' : 'Removed token from'} favorites`
    );
  };
}
