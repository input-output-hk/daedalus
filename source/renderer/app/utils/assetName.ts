import { hexToPrintableAsciiString } from './strings';
import type { AssetMetadata } from '../api/assets/types';
import type { AssetMetadataSource } from '../../../common/types/asset-metadata.types';

/**
 * Where a displayed asset name came from.
 *
 * A registry name was published by an issuer against the token's minting
 * policy. A chain name is in the transaction that minted the asset, which had
 * to satisfy that policy, so it is bound to the policy too. A minter-chosen
 * name is the asset's own name bytes decoded as text, and those bytes are
 * whatever the minter put there: an asset whose name bytes spell an existing
 * ticker is free to exist. The last must never render like the first two.
 */
export enum AssetNameProvenance {
  RegistryTicker = 'registryTicker',
  RegistryName = 'registryName',
  ChainName = 'chainName',
  MinterChosen = 'minterChosen',
}
export type ResolvedAssetName = {
  name: string;
  provenance: AssetNameProvenance;
};
type ResolvableAsset = {
  assetName?: string | null;
  metadata?: AssetMetadata | null;
  source?: AssetMetadataSource | null;
};

/**
 * Resolves the name to display for an asset, highest source first: the registry
 * ticker, the registry name, the CIP-25 or CIP-68 name from a chain row, then
 * the asset's own name bytes when every one of them is printable ASCII, then
 * nothing.
 *
 * The row's `source` is what separates the second rung from the third. Both
 * arrive as `metadata.name`, because the cache stores one name column, and a
 * chain row is the only kind of row whose name did not come from the registry.
 * A chain row carries no ticker, so the first rung cannot be reached by one.
 *
 * Returns `null` when no rung resolves. The caller renders the fingerprint,
 * which is the identity in that case.
 */
export const resolveAssetName = ({
  assetName,
  metadata,
  source,
}: ResolvableAsset): ResolvedAssetName | null => {
  if (metadata?.ticker) {
    return {
      name: metadata.ticker,
      provenance: AssetNameProvenance.RegistryTicker,
    };
  }

  if (metadata?.name) {
    return {
      name: metadata.name,
      provenance:
        source === 'chain'
          ? AssetNameProvenance.ChainName
          : AssetNameProvenance.RegistryName,
    };
  }

  const decodedAssetName = hexToPrintableAsciiString(assetName);

  if (decodedAssetName) {
    return {
      name: decodedAssetName,
      provenance: AssetNameProvenance.MinterChosen,
    };
  }

  return null;
};

/**
 * Whether a resolved name was chosen by the minter rather than published by an
 * issuer. Every surface that renders a name marks this case.
 */
export const isMinterChosenAssetName = (
  resolved: ResolvedAssetName | null
): boolean => resolved?.provenance === AssetNameProvenance.MinterChosen;
