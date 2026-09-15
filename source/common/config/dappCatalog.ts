import { MAINNET, MAINNET_FLIGHT } from '../types/environment.types';
import type { Network } from '../types/environment.types';

import type {
  DappCatalogEntry,
  DappCatalogPresentationEntry,
} from '../types/dapp.types';

export type { DappCatalogPresentationEntry } from '../types/dapp.types';

export const defineDappCatalog = (
  entries: readonly DappCatalogEntry[]
): readonly DappCatalogEntry[] => {
  const ids = new Set<string>();
  for (const entry of entries) {
    if (!entry.id || ids.has(entry.id))
      throw new Error('DApp catalog IDs must be unique');
    ids.add(entry.id);
    if (!Array.isArray(entry.availableIn))
      throw new Error('Invalid dApp catalog availability');
  }
  return Object.freeze(
    entries.map((entry) =>
      Object.freeze({
        ...entry,
        availableIn: Object.freeze([...entry.availableIn]),
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
    availableIn: ['mainnet', 'mainnet_flight'],
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
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
  {
    id: 'unfrack-it',
    availableIn: ['mainnet', 'mainnet_flight', 'preprod', 'preview'],
    nameMessageId: 'dapp.catalog.unfrack.name',
    descriptionMessageId: 'dapp.catalog.unfrack.description',
    iconAsset: 'unfrack',
    entryUrlByNetworkGenesis: { '*': 'https://unfrack.it/' },
    canonicalOrigin: 'https://unfrack.it',
    allowedResourceOrigins: [
      'https://fonts.googleapis.com',
      'https://fonts.gstatic.com',
      'https://cdn.jsdelivr.net',
      'https://api.koios.rest',
      'https://preprod.koios.rest',
      'https://preview.koios.rest',
    ],
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
  {
    id: 'strike-finance-mainnet',
    availableIn: ['mainnet', 'mainnet_flight'],
    nameMessageId: 'dapp.catalog.strike.name',
    descriptionMessageId: 'dapp.catalog.strike.description',
    iconAsset: 'strike',
    entryUrlByNetworkGenesis: { '*': 'https://api.strikefinance.org' },
    canonicalOrigin: 'https://api.strikefinance.org',
    allowedResourceOrigins: [],
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
  {
    id: 'strike-finance-preprod',
    availableIn: ['preprod'],
    nameMessageId: 'dapp.catalog.strike.name',
    descriptionMessageId: 'dapp.catalog.strike.description',
    iconAsset: 'strike',
    entryUrlByNetworkGenesis: {
      '*': 'https://testnet.strikefinance.org/',
    },
    canonicalOrigin: 'https://testnet.strikefinance.org',
    allowedResourceOrigins: [],
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
  {
    id: 'fluidtokens-mainnet',
    availableIn: ['mainnet', 'mainnet_flight'],
    nameMessageId: 'dapp.catalog.fluidtokens.name',
    descriptionMessageId: 'dapp.catalog.fluidtokens.description',
    iconAsset: 'fluidtokens',
    entryUrlByNetworkGenesis: { '*': 'https://app.fluidtokens.com/' },
    canonicalOrigin: 'https://app.fluidtokens.com',
    allowedResourceOrigins: [],
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
  {
    id: 'steelswap-mainnet',
    availableIn: ['mainnet', 'mainnet_flight'],
    nameMessageId: 'dapp.catalog.steelswap.name',
    descriptionMessageId: 'dapp.catalog.steelswap.description',
    iconAsset: 'steelswap',
    entryUrlByNetworkGenesis: { '*': 'https://steelswap.io/' },
    canonicalOrigin: 'https://steelswap.io',
    allowedResourceOrigins: [
      'https://api.steelswap.io',
      'https://fonts.googleapis.com',
      'https://fonts.gstatic.com',
      'https://gt1.dezons.com',
    ],
    supportedWalletKinds: ['shelley-software', 'ledger', 'trezor'],
    supportedExtensions: [],
  },
]);

export const getDappCatalog = (
  network: Network,
  isFlight: boolean,
  catalog: readonly DappCatalogEntry[] = dappCatalog
): readonly DappCatalogEntry[] => {
  const variant = network === MAINNET && isFlight ? MAINNET_FLIGHT : network;
  return Object.freeze(
    catalog.filter(({ availableIn }) =>
      availableIn.some((candidate) => candidate === variant)
    )
  );
};

export const getDappCatalogPresentation = (
  network: Network,
  isFlight: boolean
): readonly DappCatalogPresentationEntry[] =>
  Object.freeze(
    getDappCatalog(
      network,
      isFlight
    ).map(({ id, nameMessageId, descriptionMessageId, iconAsset }) =>
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
