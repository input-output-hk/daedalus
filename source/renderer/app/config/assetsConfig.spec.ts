import {
  ASSET_METADATA_SERVERS_LIST,
  ASSET_METADATA_SOURCE_TYPES,
  ASSET_METADATA_URL_VALIDATOR,
} from './assetsConfig';

describe('ASSET_METADATA_URL_VALIDATOR', () => {
  it('accepts the Koios default with its path prefix', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://api.koios.rest/api/v1')
    ).toBe(true);
  });

  it('accepts a custom instance carrying a port', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://koios.example.com:8443')
    ).toBe(true);
  });

  it('accepts a custom instance carrying a port and a path', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://koios.example.com:8443/api/v1')
    ).toBe(true);
  });

  it('accepts a path with a trailing slash', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://api.koios.rest/api/v1/')
    ).toBe(true);
  });

  it('accepts the literal direct', () => {
    expect(ASSET_METADATA_URL_VALIDATOR.test('direct')).toBe(true);
  });

  it('rejects an http URL', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('http://api.koios.rest/api/v1')
    ).toBe(false);
  });

  it('rejects a URL carrying a query string', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://api.koios.rest/api/v1?x=1')
    ).toBe(false);
  });

  it('rejects a host carrying a character outside the class', () => {
    expect(
      ASSET_METADATA_URL_VALIDATOR.test('https://api.koios.rest%2f/api/v1')
    ).toBe(false);
  });

  it('rejects the empty string', () => {
    expect(ASSET_METADATA_URL_VALIDATOR.test('')).toBe(false);
  });
});

describe('ASSET_METADATA_SERVERS_LIST', () => {
  it('holds the two presets that have a fixed URL', () => {
    expect(Object.keys(ASSET_METADATA_SERVERS_LIST).sort()).toEqual([
      'direct',
      'koios',
    ]);
  });

  // Anything that reduces over the list and does not match falls back to
  // CUSTOM. A `custom` entry here would match on its own placeholder URL.
  it('holds no custom entry', () => {
    expect(ASSET_METADATA_SERVERS_LIST.custom).toBeUndefined();
  });

  it('carries the literal direct as the direct option URL', () => {
    expect(ASSET_METADATA_SERVERS_LIST.direct.url).toBe('direct');
  });
});

describe('ASSET_METADATA_SOURCE_TYPES', () => {
  it('holds all three source types', () => {
    expect(ASSET_METADATA_SOURCE_TYPES).toEqual({
      KOIOS: 'koios',
      CUSTOM: 'custom',
      DIRECT: 'direct',
    });
  });
});
