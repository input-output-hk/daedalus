import {
  MAINNET,
  MAINNET_FLIGHT,
} from '../../../../common/types/environment.types';

/**
 * Which network a Cardano address belongs to, from its bech32 prefix alone.
 *
 * A payment address published in DRep metadata is a string the DRep chose, and
 * nothing checks it: an address for the wrong network reaches the page looking
 * exactly like a usable one. Somebody who copies it and sends to it has sent to
 * an address their own network cannot reach.
 *
 * The check costs a string comparison. Cardano's bech32 human-readable part
 * already carries the network, `addr` on mainnet and `addr_test` everywhere
 * else, so no decoding, no round trip to the wallet backend and no dependency
 * are needed to tell the two apart. It does not establish that the address is
 * well formed, only which network it claims: a full validation is the backend's
 * job and costs a request.
 */
export type AddressNetwork = 'mainnet' | 'testnet' | 'unknown';

const MAINNET_NETWORKS: ReadonlySet<string> = new Set([
  MAINNET,
  MAINNET_FLIGHT,
]);

export function getAddressNetwork(address: string): AddressNetwork {
  if (typeof address !== 'string') return 'unknown';
  const value = address.trim().toLowerCase();
  // Order matters: every testnet prefix also starts with its mainnet one.
  if (value.startsWith('addr_test1') || value.startsWith('stake_test1')) {
    return 'testnet';
  }
  if (value.startsWith('addr1') || value.startsWith('stake1')) return 'mainnet';
  return 'unknown';
}

/**
 * Whether an address belongs to the network the wallet is running against.
 *
 * Null rather than false when there is nothing to compare: an unrecognised
 * prefix or an unknown network is not evidence of a mismatch, and warning on it
 * would put a notice under every address the moment a new prefix appears.
 */
export function isAddressForNetwork(
  address: string,
  network: string | null | undefined
): boolean | null {
  if (!network) return null;
  const addressNetwork = getAddressNetwork(address);
  if (addressNetwork === 'unknown') return null;
  return (
    addressNetwork === (MAINNET_NETWORKS.has(network) ? 'mainnet' : 'testnet')
  );
}
