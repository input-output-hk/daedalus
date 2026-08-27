export type DappScope =
  | 'connection'
  | 'read'
  | 'transaction-signing'
  | 'data-signing'
  | 'transaction-submission'
  | 'governance-key-disclosure'
  | 'governance-data-signing'
  | 'governance-transaction-signing'
  | 'account-public-key-disclosure';

export type DappCatalogEntry = Readonly<{
  id: string;
  nameMessageId: string;
  descriptionMessageId: string;
  iconAsset: string;
  entryUrlByNetworkGenesis: Readonly<Record<string, string>>;
  canonicalOrigin: string;
  allowedResourceOrigins: readonly string[];
  supportedWalletKinds: readonly string[];
  supportedExtensions: readonly number[];
}>;

export type DappCatalogPresentationEntry = Readonly<{
  id: string;
  nameMessageId: string;
  descriptionMessageId: string;
  iconAsset: string;
}>;

export type DappGrantLaunch =
  | Readonly<{
      kind: 'catalog';
      catalogEntryId: string;
      catalogEntryIdentity: string;
    }>
  | Readonly<{ kind: 'diagnostics' }>;

export type DappGrant = Readonly<{
  schemaVersion: 1;
  origin: string;
  walletId: string;
  networkGenesis: string;
  networkMagic: number;
  readScopes: readonly DappScope[];
  enabledExtensionScopes: readonly number[];
  launch: DappGrantLaunch;
  grantedAt: string;
}>;

export type DappCapability = Readonly<{
  guestWebContentsId: number;
  documentGeneration: number;
  dappId?: string;
  origin: string;
  connectionId: string;
  walletId: string;
  routeEpoch: number;
  networkId: number;
  networkMagic: number;
  networkGenesis: string;
  enabledExtensions: readonly number[];
  grantedScopes: readonly DappScope[];
}>;
