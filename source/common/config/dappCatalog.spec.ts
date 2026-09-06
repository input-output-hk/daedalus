import {
  DAPP_CATALOG_REVISION,
  defineDappCatalog,
  dappCatalog,
  dappCatalogPresentation,
  findDappCatalogEntry,
} from './dappCatalog';
import type { DappCatalogEntry } from '../types/dapp.types';

const entry = (id: string): DappCatalogEntry => ({
  id,
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
  it('ships the Mainnet Liqwid Ledger pilot and exposes only presentation fields', () => {
    expect(DAPP_CATALOG_REVISION).toBe(2);
    expect(dappCatalog).toEqual([
      expect.objectContaining({
        id: 'liqwid-finance',
        entryUrlByNetworkGenesis: {
          '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb':
            'https://app.liqwid.finance/',
        },
        supportedWalletKinds: ['ledger'],
        supportedExtensions: [],
      }),
    ]);
    expect(dappCatalogPresentation).toEqual([
      {
        id: 'liqwid-finance',
        nameMessageId: 'dapp.catalog.liqwid.name',
        descriptionMessageId: 'dapp.catalog.liqwid.description',
        iconAsset: 'liqwid',
      },
    ]);
  });

  it('rejects duplicate IDs and resolves injected entries by opaque ID', () => {
    expect(() => defineDappCatalog([entry('same'), entry('same')])).toThrow(
      'unique'
    );
    expect(
      findDappCatalogEntry(defineDappCatalog([entry('one')]), 'one')
    ).toEqual(expect.objectContaining({ id: 'one' }));
    expect(() => findDappCatalogEntry([], 'missing')).toThrow('Unknown');
  });
});
