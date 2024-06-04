import _ from 'lodash';
import { bech32 } from 'bech32';
import { str_to_path } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import { HexString } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/types/internal';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { deriveXpubChannel } from '../ipc/getHardwareWalletChannel';
import { HARDENED } from '../config/hardwareWalletsConfig';
// Types
import type { CoinSelectionAssetsType } from '../api/transactions/types';
import type { AddressType } from '../../../common/types/address-introspection.types';

export type PathRoleIdentityType =
  | 'utxo_external'
  | 'utxo_internal'
  | 'mutable_account'
  | 'multisig_script';
// Constants
export const CERTIFICATE_TYPE = {
  register_reward_account: 0,
  // register_reward_account
  quit_pool: 1,
  // quit_pool
  join_pool: 2, // join_pool
};
export const PATH_ROLE_IDENTITY = {
  role0: 'utxo_external',
  // address
  role1: 'utxo_internal',
  // change
  role2: 'mutable_account',
  // stake
  role3: 'multisig_script', // script
};
// See src/cardano.h in https://github.com/vacuumlabs/ledger-app-cardano-shelley
export const MAX_HUMAN_ADDRESS_LENGTH = 150;
// https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md
export const KEY_PREFIXES = {
  // ...add more keys if needed
  PUBLIC_KEY_WITH_CHAIN_CODE: 'acct_xvk', // Ed25519 public key with chain code
};
// Helpers
const receiverAddressTypes: Set<AddressType> = new Set([
  0,
  1,
  2,
  3,
  4,
  5,
  6,
  7,
  8,
]);
export const CATALYST_VOTING_REGISTRATION_TYPE = 'CATALYST_VOTING';
export const HARDENED_THRESHOLD = 0x80000000;
export const derivationScheme = {
  type: 'v2',
  ed25519Mode: 2,
  keyfileVersion: '2.0.0',
};

export const isReceiverAddressType = (addressType: AddressType) =>
  receiverAddressTypes.has(addressType);
// [1852H, 1815H, 0H] => m/1852'/1815'/0'
export const derivationPathToString = (derivationPath: Array<string>) => {
  let constructedPath = 'm';

  _.map(derivationPath, (chunk) => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  });

  return constructedPath;
};
// m/1852'/1815'/0' => 1852'/1815'/0'
export const derivationPathToAddressPath = (derivationPath: Array<string>) => {
  const derivationPathString = derivationPathToString(derivationPath);
  const addressPath = derivationPathString.replace('m/', '');
  return addressPath;
};
// [1852H, 1815H, 0H] => [2147485500, 2147485463, 2147483648]
export const derivationPathToLedgerPath = (derivationPath: Array<string>) => {
  const transformedPath = _.map(derivationPath, (chunk) =>
    chunk.replace('H', "'")
  );

  const constructedPath = _.join(transformedPath, '/');

  return str_to_path(constructedPath);
};
export const getParamsFromPath = (derivationPath: Array<string>) => {
  const pathParams = _.takeRight(derivationPath, 2);

  return {
    role: pathParams[0],
    index: pathParams[1],
    roleIdentity: PATH_ROLE_IDENTITY[`role${pathParams[0]}`],
  };
};
// [2147485500, 2147485463, 2147483648] => 1852'/1815'/0'
export const hardenedPathToString = (hardendedPath: Array<number>) => {
  const path = _.map(hardendedPath, (chunk) => `${chunk - HARDENED}H`);

  return derivationPathToString(path).replace('m/', '');
};
// [2147485500, 2147485463, 2147483648] => [1852H, 1815H, 0H, 0, 1]
export const hardenedPathToDerivationPath = (hardendedPath: Array<number>) => {
  const derivationPath = [];
  const constructedDerivationPath = ['m'];

  _.map(hardendedPath, (chunk, index) => {
    let pathChunk = chunk.toString();
    let constructedPathChunk = chunk.toString();

    if (index <= 2) {
      pathChunk = `${chunk - HARDENED}H`;
      constructedPathChunk = `${chunk - HARDENED}'`;
    }

    derivationPath.push(pathChunk);
    constructedDerivationPath.push(constructedPathChunk);
  });

  return {
    derivationPath,
    constructed: constructedDerivationPath,
  };
};
export const bech32EncodePublicKey = (data: Buffer): string => {
  const data5bit = bech32.toWords(data);
  return bech32.encode(
    KEY_PREFIXES.PUBLIC_KEY_WITH_CHAIN_CODE,
    data5bit,
    MAX_HUMAN_ADDRESS_LENGTH
  );
};
export const bech32DecodePublicKey = (data: string): Buffer => {
  const { words } = bech32.decode(data, 1000);
  return Buffer.from(bech32.fromWords(words));
};
export const groupTokensByPolicyId = (assets: CoinSelectionAssetsType) => {
  const compareStringsCanonically = (string1: string, string2: string) =>
    string1.length - string2.length || string1.localeCompare(string2);

  const groupedAssets = {};

  _(assets)
    .orderBy(['policyId', 'assetName'], ['asc', 'asc'])
    .groupBy(({ policyId }) => policyId)
    .mapValues((tokens) =>
      tokens.map(({ assetName, quantity, policyId }) => ({
        assetName,
        quantity,
        policyId,
      }))
    )
    .map((tokens, policyId) => ({
      policyId,
      assets: tokens.sort((token1, token2) =>
        compareStringsCanonically(token1.assetName, token2.assetName)
      ),
    }))
    .sort((token1, token2) =>
      compareStringsCanonically(token1.policyId, token2.policyId)
    )
    .value()
    .map((sortedAssetsGroup) => {
      groupedAssets[sortedAssetsGroup.policyId] = sortedAssetsGroup.assets;
      return groupedAssets;
    });

  return groupedAssets;
};

export const indexIsHardened = (index: number) => {
  return index >= HARDENED_THRESHOLD;
};

export const CachedDeriveXpubFactory = (
  deriveXpubHardenedFn: (...args: Array<any>) => any
) => {
  const derivedXpubs = {};
  let xpubMemo;

  const deriveXpub = async (
    absDerivationPath: Array<number>,
    xpubHex: string | null | undefined
  ) => {
    if (xpubHex) xpubMemo = xpubHex;
    const memoKey = JSON.stringify(absDerivationPath);
    let derivedXpubsMemo = await derivedXpubs[memoKey];

    if (!derivedXpubsMemo) {
      const deriveHardened =
        absDerivationPath.length === 0 ||
        indexIsHardened(absDerivationPath.slice(-1)[0]);
      derivedXpubsMemo = deriveHardened
        ? await deriveXpubHardenedFn(xpubMemo)
        : await deriveXpubNonhardenedFn(absDerivationPath);
    }

    /*
     * the derivedXpubs map stores promises instead of direct results
     * to deal with concurrent requests to derive the same xpub
     */
    return derivedXpubsMemo;
  };

  const deriveXpubNonhardenedFn = async (derivationPath) => {
    const lastIndex = derivationPath.slice(-1)[0];
    const parentXpub = await deriveXpub(derivationPath.slice(0, -1), null);

    try {
      const parentXpubHex = utils.buf_to_hex(parentXpub);
      const derivedXpub = await deriveXpubChannel.request({
        parentXpubHex,
        lastIndex,
        derivationScheme: derivationScheme.ed25519Mode,
      });
      return utils.hex_to_buf(derivedXpub as HexString);
    } catch (e) {
      throw e;
    }
  };

  return deriveXpub;
};

export const deriveXpub = CachedDeriveXpubFactory(async (xpubHex) => {
  return Buffer.from(xpubHex, 'hex');
});
