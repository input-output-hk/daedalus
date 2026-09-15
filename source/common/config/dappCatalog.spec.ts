import {
  defineDappCatalog,
  dappCatalog,
  findDappCatalogEntry,
  getDappCatalog,
  getDappCatalogPresentation,
} from './dappCatalog';
import type { DappCatalogEntry } from '../types/dapp.types';
import type { Network } from '../types/environment.types';

const entry = (
  id: string,
  availableIn: DappCatalogEntry['availableIn'] = [
    'mainnet',
    'mainnet_flight',
    'preprod',
    'preview',
  ]
): DappCatalogEntry => ({
  id,
  availableIn,
  nameMessageId: `${id}.name`,
  descriptionMessageId: `${id}.description`,
  iconAsset: `${id}.svg`,
  entryUrlByNetworkGenesis: { genesis: 'https://example.com' },
  canonicalOrigin: 'https://example.com',
  allowedResourceOrigins: [],
  supportedWalletKinds: [],
  supportedExtensions: [],
});

describe('dapp catalog', () => {
  it('selects entries by the effective Daedalus variant', () => {
    const networks: Network[] = [
      'mainnet',
      'mainnet_flight',
      'testnet',
      'staging',
      'shelley_qa',
      'alonzo_purple',
      'vasil_dev',
      'preprod',
      'preview',
      'selfnode',
      'development',
    ];
    for (const network of networks)
      expect(getDappCatalog(network, false).map(({ id }) => id)).toEqual(
        network === 'mainnet' || network === 'mainnet_flight'
          ? [
              'liqwid-finance',
              'unfrack-it',
              'strike-finance-mainnet',
              'fluidtokens-mainnet',
              'steelswap-mainnet',
            ]
          : network === 'preprod'
          ? ['unfrack-it', 'strike-finance-preprod']
          : network === 'preview'
          ? ['unfrack-it']
          : []
      );
    expect(
      findDappCatalogEntry(dappCatalog, 'strike-finance-mainnet')
        .entryUrlByNetworkGenesis['*']
    ).toBe('https://api.strikefinance.org');
    expect(
      findDappCatalogEntry(dappCatalog, 'strike-finance-preprod')
        .entryUrlByNetworkGenesis['*']
    ).toBe('https://testnet.strikefinance.org/');
    expect(
      findDappCatalogEntry(dappCatalog, 'fluidtokens-mainnet')
        .entryUrlByNetworkGenesis['*']
    ).toBe('https://app.fluidtokens.com/');
    expect(
      findDappCatalogEntry(dappCatalog, 'steelswap-mainnet')
        .entryUrlByNetworkGenesis['*']
    ).toBe('https://steelswap.io/');
    expect(
      findDappCatalogEntry(dappCatalog, 'steelswap-mainnet')
        .allowedResourceOrigins
    ).toEqual([
      'https://api.steelswap.io',
      'https://fonts.googleapis.com',
      'https://fonts.gstatic.com',
      'https://gt1.dezons.com',
    ]);

    const injected = defineDappCatalog([
      entry('supported'),
      entry('flight', ['mainnet_flight']),
      entry('disabled', []),
    ]);
    expect(
      getDappCatalog('mainnet', false, injected).map(({ id }) => id)
    ).toEqual(['supported']);
    expect(
      getDappCatalog('mainnet', true, injected).map(({ id }) => id)
    ).toEqual(['supported', 'flight']);
    expect(
      getDappCatalog('preprod', true, injected).map(({ id }) => id)
    ).toEqual(['supported']);
  });

  it('rejects malformed catalogs and resolves entries by opaque ID', () => {
    expect(() => defineDappCatalog([entry('same'), entry('same')])).toThrow(
      'unique'
    );
    const invalid = { ...entry('invalid'), availableIn: undefined };
    expect(() =>
      defineDappCatalog([(invalid as unknown) as DappCatalogEntry])
    ).toThrow('Invalid dApp catalog availability');
    expect(
      findDappCatalogEntry(defineDappCatalog([entry('one')]), 'one')
    ).toEqual(expect.objectContaining({ id: 'one' }));
    expect(() => findDappCatalogEntry([], 'missing')).toThrow('Unknown');
  });
  it('projects only renderer presentation fields', () => {
    expect(getDappCatalogPresentation('mainnet', false)).toEqual([
      {
        id: 'liqwid-finance',
        nameMessageId: 'dapp.catalog.liqwid.name',
        descriptionMessageId: 'dapp.catalog.liqwid.description',
        iconAsset: 'liqwid',
      },
      {
        id: 'unfrack-it',
        nameMessageId: 'dapp.catalog.unfrack.name',
        descriptionMessageId: 'dapp.catalog.unfrack.description',
        iconAsset: 'unfrack',
      },
      {
        id: 'strike-finance-mainnet',
        nameMessageId: 'dapp.catalog.strike.name',
        descriptionMessageId: 'dapp.catalog.strike.description',
        iconAsset: 'strike',
      },
      {
        id: 'fluidtokens-mainnet',
        nameMessageId: 'dapp.catalog.fluidtokens.name',
        descriptionMessageId: 'dapp.catalog.fluidtokens.description',
        iconAsset: 'fluidtokens',
      },
      {
        id: 'steelswap-mainnet',
        nameMessageId: 'dapp.catalog.steelswap.name',
        descriptionMessageId: 'dapp.catalog.steelswap.description',
        iconAsset: 'steelswap',
      },
    ]);
  });
});
