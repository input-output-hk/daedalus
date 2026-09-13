import { dappCatalog } from '../../common/config/dappCatalog';
import type { DappCatalogEntry } from '../../common/types/dapp.types';
import { dappCatalogEntryIdentity, resolveCatalogLaunch } from './dappCatalog';

const entry: DappCatalogEntry = {
  id: 'example',
  availableIn: ['preprod'],
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

  it('resolves unfrack.it through its universal configured URL', () => {
    for (const genesis of [
      '5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb',
      'arbitrary-selfnode-genesis',
    ])
      expect(
        resolveCatalogLaunch(dappCatalog[1], genesis, 'unfrack.it').entryUrl
      ).toBe('https://unfrack.it/');
    expect(() =>
      resolveCatalogLaunch(
        dappCatalog[0],
        'arbitrary-selfnode-genesis',
        'Liqwid Finance'
      )
    ).toThrow();
  });

  it('prefers an exact URL without falling back after validation', () => {
    expect(
      resolveCatalogLaunch(
        {
          ...entry,
          entryUrlByNetworkGenesis: {
            genesis: 'https://example.com/exact',
            '*': 'https://example.com/universal',
          },
        },
        'genesis',
        'Example'
      ).entryUrl
    ).toBe('https://example.com/exact');
    expect(() =>
      resolveCatalogLaunch(
        {
          ...entry,
          entryUrlByNetworkGenesis: {
            genesis: 'https://other.example/exact',
            '*': 'https://example.com/universal',
          },
        },
        'genesis',
        'Example'
      )
    ).toThrow('DApp catalog origin mismatch');
  });

  it('changes grant identity when the localized description contract changes', () => {
    expect(
      dappCatalogEntryIdentity({
        ...entry,
        descriptionMessageId: 'example.changed-description',
      })
    ).not.toBe(dappCatalogEntryIdentity(entry));
  });

  it('changes identity only when availability membership changes', () => {
    expect(
      dappCatalogEntryIdentity({
        ...entry,
        availableIn: ['preprod', 'preview'],
      })
    ).toBe(
      dappCatalogEntryIdentity({
        ...entry,
        availableIn: ['preview', 'preprod'],
      })
    );
    expect(
      dappCatalogEntryIdentity({ ...entry, availableIn: ['preview'] })
    ).not.toBe(dappCatalogEntryIdentity(entry));
  });
});
