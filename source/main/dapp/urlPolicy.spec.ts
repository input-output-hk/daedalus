import { resolveCatalogLaunch } from './dappCatalog';
import type { DappCatalogEntry } from './dappCatalog';
import {
  canonicalizeDappOrigin,
  isAllowedDappResourceUrl,
  isAllowedDiagnosticsResourceUrl,
  parseDappUrl,
  parseDiagnosticsDappUrl,
} from './urlPolicy';

const entry: DappCatalogEntry = {
  id: 'example',
  nameMessageId: 'dapp.example.name',
  descriptionMessageId: 'dapp.example.description',
  iconAsset: 'example.svg',
  entryUrlByNetworkGenesis: { genesis: 'https://example.com/app' },
  canonicalOrigin: 'https://example.com',
  allowedResourceOrigins: ['https://cdn.example.com', 'wss://ws.example.com'],
  supportedWalletKinds: ['shelley'],
  supportedExtensions: [95],
};

describe('dApp URL and catalog policy', () => {
  test('canonicalizes secure URLs and effective ports', () => {
    expect(parseDappUrl('https://bücher.example:443/app')).toEqual({
      href: 'https://xn--bcher-kva.example/app',
      origin: 'https://xn--bcher-kva.example',
    });
    expect(canonicalizeDappOrigin('https://example.com:443')).toBe(
      'https://example.com'
    );
  });

  test.each([
    'http://example.com',
    'https://user@example.com',
    'https://example.com:pass@example.org',
    'file:///tmp/dapp.html',
    'data:text/html,dapp',
    'blob:https://example.com/id',
    'mailto:test@example.com',
    'not a url',
  ])('rejects %s', (value) => {
    expect(() => parseDappUrl(value)).toThrow('Invalid dApp URL');
  });

  test('limits development HTTP to explicit loopback policy', () => {
    const development = { allowHttpLoopback: true };
    expect(
      parseDiagnosticsDappUrl('http://localhost:3000/app', development)
    ).toEqual({
      href: 'http://localhost:3000/app',
      origin: 'http://localhost:3000',
    });
    expect(
      parseDiagnosticsDappUrl('http://[::1]:3000/app', development).origin
    ).toBe('http://[::1]:3000');
    expect(() =>
      parseDiagnosticsDappUrl('http://example.com', development)
    ).toThrow('Invalid dApp URL');
    expect(() =>
      parseDiagnosticsDappUrl('http://127.0.0.1', {
        allowHttpLoopback: false,
      })
    ).toThrow('Invalid dApp URL');
    expect(canonicalizeDappOrigin('http://LOCALHOST:3000', development)).toBe(
      'http://localhost:3000'
    );
  });

  test('allows only secure or explicit development-loopback diagnostics resources', () => {
    expect(
      isAllowedDiagnosticsResourceUrl('https://example.com/a', {
        allowHttpLoopback: false,
      })
    ).toBe(true);
    expect(
      isAllowedDiagnosticsResourceUrl('wss://ws.example.com/a', {
        allowHttpLoopback: false,
      })
    ).toBe(true);
    expect(
      isAllowedDiagnosticsResourceUrl('http://localhost:3000/a', {
        allowHttpLoopback: true,
      })
    ).toBe(true);
    expect(
      isAllowedDiagnosticsResourceUrl('http://example.com/a', {
        allowHttpLoopback: true,
      })
    ).toBe(false);
    expect(
      isAllowedDiagnosticsResourceUrl('file:///tmp/dapp.html', {
        allowHttpLoopback: true,
      })
    ).toBe(false);
  });

  test('matches only exact secure resource origins', () => {
    const allowed = new Set(['https://example.com', 'wss://ws.example.com']);
    expect(isAllowedDappResourceUrl('https://example.com/a', allowed)).toBe(
      true
    );
    expect(
      isAllowedDappResourceUrl('wss://ws.example.com/socket', allowed)
    ).toBe(true);
    expect(
      isAllowedDappResourceUrl('https://example.com.evil.test', allowed)
    ).toBe(false);
    expect(isAllowedDappResourceUrl('http://example.com', allowed)).toBe(false);
  });

  test('resolves a network URL and local title without page input', () => {
    const launch = resolveCatalogLaunch(entry, 'genesis', 'Example');
    expect(launch).toMatchObject({
      catalogId: 'example',
      entryUrl: 'https://example.com/app',
      canonicalOrigin: 'https://example.com',
      windowTitle: 'Example — Daedalus',
    });
    expect([...launch.allowedResourceOrigins]).toEqual([
      'https://cdn.example.com',
      'wss://ws.example.com',
      'https://example.com',
    ]);
  });

  test('rejects network, origin, and local-title mismatches', () => {
    expect(() => resolveCatalogLaunch(entry, 'other', 'Example')).toThrow();
    expect(() =>
      resolveCatalogLaunch(
        { ...entry, canonicalOrigin: 'https://example.org' },
        'genesis',
        'Example'
      )
    ).toThrow('DApp catalog origin mismatch');
    expect(() => resolveCatalogLaunch(entry, 'genesis', '\n')).toThrow(
      'Invalid local dApp title'
    );
  });
});
