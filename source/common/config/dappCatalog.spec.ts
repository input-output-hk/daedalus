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
  it('ships an explicit empty revision and exposes only presentation fields', () => {
    expect(DAPP_CATALOG_REVISION).toBe(1);
    expect(dappCatalog).toEqual([]);
    expect(dappCatalogPresentation).toEqual([]);
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
