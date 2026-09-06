import type {
  DappCatalogEntry,
  DappCatalogPresentationEntry,
} from '../types/dapp.types';

export type { DappCatalogPresentationEntry } from '../types/dapp.types';

export const DAPP_CATALOG_REVISION = 2;

export const defineDappCatalog = (
  entries: readonly DappCatalogEntry[]
): readonly DappCatalogEntry[] => {
  const ids = new Set<string>();
  for (const entry of entries) {
    if (!entry.id || ids.has(entry.id))
      throw new Error('DApp catalog IDs must be unique');
    ids.add(entry.id);
  }
  return Object.freeze(
    entries.map((entry) =>
      Object.freeze({
        ...entry,
        entryUrlByNetworkGenesis: Object.freeze({
          ...entry.entryUrlByNetworkGenesis,
        }),
        allowedResourceOrigins: Object.freeze([
          ...entry.allowedResourceOrigins,
        ]),
        supportedWalletKinds: Object.freeze([...entry.supportedWalletKinds]),
        supportedExtensions: Object.freeze([...entry.supportedExtensions]),
      })
    )
  );
};

export const dappCatalog = defineDappCatalog([
  {
    id: 'liqwid-finance',
    nameMessageId: 'dapp.catalog.liqwid.name',
    descriptionMessageId: 'dapp.catalog.liqwid.description',
    iconAsset: 'liqwid',
    entryUrlByNetworkGenesis: {
      '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb':
        'https://app.liqwid.finance/',
    },
    canonicalOrigin: 'https://app.liqwid.finance',
    allowedResourceOrigins: [
      'https://key-value-storage.liqwid.finance',
      'https://public.liqwid.finance',
      'https://v2.api.liqwid.finance',
    ],
    supportedWalletKinds: ['ledger'],
    supportedExtensions: [],
  },
]);

export const dappCatalogPresentation: readonly DappCatalogPresentationEntry[] = Object.freeze(
  dappCatalog.map(({ id, nameMessageId, descriptionMessageId, iconAsset }) =>
    Object.freeze({ id, nameMessageId, descriptionMessageId, iconAsset })
  )
);

export const findDappCatalogEntry = (
  catalog: readonly DappCatalogEntry[],
  id: string
): DappCatalogEntry => {
  if (typeof id !== 'string' || id === '')
    throw new Error('Unknown dApp catalog entry');
  const entry = catalog.find((candidate) => candidate.id === id);
  if (!entry) throw new Error('Unknown dApp catalog entry');
  return entry;
};
