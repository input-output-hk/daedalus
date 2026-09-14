import { hexToPrintableAsciiString } from './strings';
import type { AssetMetadata } from '../api/assets/types';

/**
 * Where a displayed asset name came from.
 *
 * A registry name was published by an issuer against the token's minting
 * policy. A minter-chosen name is the asset's own name bytes decoded as text,
 * and those bytes are whatever the minter put there: an asset whose name bytes
 * spell an existing ticker is free to exist. The two must never render alike.
 */
export enum AssetNameProvenance {
  RegistryTicker = 'registryTicker',
  RegistryName = 'registryName',
  MinterChosen = 'minterChosen',
}
export type ResolvedAssetName = {
  name: string;
  provenance: AssetNameProvenance;
};
type ResolvableAsset = {
  assetName?: string | null;
  metadata?: AssetMetadata | null;
};

/**
 * Resolves the name to display for an asset, highest source first: the registry
 * ticker, the registry name, then the asset's own name bytes when every one of
 * them is printable ASCII, then nothing.
 *
 * A CIP-25 or CIP-68 name sits between the registry name and the decoded name
 * once the chain channel can resolve one. Nothing produces one yet.
 *
 * Returns `null` when no rung resolves. The caller renders the fingerprint,
 * which is the identity in that case.
 */
export const resolveAssetName = ({
  assetName,
  metadata,
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
      provenance: AssetNameProvenance.RegistryName,
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
