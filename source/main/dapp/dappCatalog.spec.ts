import { dappCatalog } from '../../common/config/dappCatalog';
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

  it('resolves the Mainnet Liqwid pilot with its exact egress allowlist', () => {
    const launch = resolveCatalogLaunch(
      dappCatalog[0],
      '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb',
      'Liqwid Finance'
    );
    expect(launch.entryUrl).toBe('https://app.liqwid.finance/');
    expect([...launch.allowedResourceOrigins].sort()).toEqual([
      'https://app.liqwid.finance',
      'https://key-value-storage.liqwid.finance',
      'https://public.liqwid.finance',
      'https://v2.api.liqwid.finance',
    ]);
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
