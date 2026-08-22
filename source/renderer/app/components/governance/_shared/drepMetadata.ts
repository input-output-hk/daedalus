import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';

export type DRepMetadataSource = Pick<AppDRepDirectoryEntry, 'verifiedName'>;

/**
 * Whether a DRep's off-chain metadata has been fetched and verified.
 *
 * An anchor is only a URL and a hash recorded on chain: its presence says the
 * DRep pointed at something, not that the something was retrieved or matched.
 * The badge and the directory filter both read this one predicate, because
 * answering the same question two ways is what let a card filtered in as having
 * metadata render as having none.
 */
export function hasVerifiedMetadata(entry: DRepMetadataSource): boolean {
  return entry.verifiedName != null;
}
