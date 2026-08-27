import type { DappCatalogEntry } from '../../common/types/dapp.types';
import { dappCatalogEntryIdentity, resolveCatalogLaunch } from './dappCatalog';

const entry: DappCatalogEntry = {
  id: 'example',
  nameMessageId: 'example.name',
  descriptionMessageId: 'example.description',
  iconAsset: 'example.svg',
  entryUrlByNetworkGenesis: { genesis: 'https://example.com/app' },
  canonicalOrigin: 'https://example.com',
  allowedResourceOrigins: [],
  supportedWalletKinds: [],
  supportedExtensions: [],
};

describe('catalog launch resolution', () => {
  it('rejects catalog entries without an exact configured network URL', () => {
    expect(() =>
      resolveCatalogLaunch(entry, 'other-genesis', 'Example')
    ).toThrow();
  });

  it('changes grant identity when the localized description contract changes', () => {
    expect(
      dappCatalogEntryIdentity({
        ...entry,
        descriptionMessageId: 'example.changed-description',
      })
    ).not.toBe(dappCatalogEntryIdentity(entry));
  });
});
