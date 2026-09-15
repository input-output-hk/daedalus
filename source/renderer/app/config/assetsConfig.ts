import type { AssetMetadataSourceType } from '../types/assetTypes';

const { koiosUrl } = global;

export const MAX_DECIMAL_PRECISION = 20;
export const DEFAULT_DECIMAL_PRECISION = 0;

/**
 * The presets, in the shape `SMASH_SERVERS_LIST` uses at
 * `config/stakingConfig.ts:12-28`. `custom` is deliberately absent: it has no
 * fixed URL, so it lives in the type map alone and everything that reduces over
 * this list falls back to it.
 *
 * `koiosUrl` is undefined on a network the launcher configures no instance for,
 * which today is selfnode. That is the real state rather than a defensive one:
 * a selfnode chain exists only on the user's machine and no public index holds
 * it.
 */
export const ASSET_METADATA_SERVERS_LIST: Partial<
  Record<
    AssetMetadataSourceType,
    {
      name: string;
      url: string;
    }
  >
> = {
  koios: {
    name: 'Koios',
    url: koiosUrl,
  },
  // Pointers derived from the chain the user already holds.
  direct: {
    name: 'direct',
    url: 'direct',
  },
};

export const ASSET_METADATA_SOURCE_TYPES: Record<
  string,
  AssetMetadataSourceType
> = {
  KOIOS: 'koios',
  CUSTOM: 'custom',
  DIRECT: 'direct',
};

/**
 * `SMASH_URL_VALIDATOR` at `config/stakingConfig.ts:43-45` with one addition,
 * and the addition is the whole difference: Koios serves under an `/api/v1`
 * path prefix, so the pattern admits path segments where the SMASH pattern
 * rejects them. `https` only, an optional port, and the literal `direct` are
 * unchanged, and a query string is still rejected because `?` and `=` are
 * outside every character class here.
 */
export const ASSET_METADATA_URL_VALIDATOR = new RegExp(
  '^(direct|https://[a-zA-Z0-9-_~.]+(:[0-9]+)?(/[a-zA-Z0-9-_~.]+)*/?)$'
);
