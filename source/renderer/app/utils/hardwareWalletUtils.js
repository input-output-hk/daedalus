// @flow
import { map, join, takeRight } from 'lodash';
import { bech32 } from 'bech32';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { HARDENED } from '../config/hardwareWalletsConfig';

// Constants
export const CERTIFICATE_TYPE = {
  register_reward_account: 0, // register_reward_account
  quit_pool: 1, // quit_pool
  join_pool: 2, // join_pool
};

export const PATH_ROLE_IDENTITY = {
  role0: 'utxo_external', // address
  role1: 'utxo_internal', // change
  role2: 'mutable_account', // stake
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

// [1852H, 1815H, 0H] => m/1852'/1815'/0'
export const derivationPathToString = (derivationPath: Array<string>) => {
  let constructedPath = 'm';
  map(derivationPath, (chunk) => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  });
  return constructedPath;
};

// // [1852H, 1815H, 0H] => [2147485500, 2147485463, 2147483648]
export const derivationPathToLedgerPath = (derivationPath: Array<string>) => {
  const transformedPath = map(derivationPath, (chunk) =>
    chunk.replace('H', "'")
  );
  const constructedPath = join(transformedPath, '/');
  return utils.str_to_path(constructedPath);
};

export const getParamsFromPath = (derivationPath: Array<string>) => {
  const pathParams = takeRight(derivationPath, 2);
  return {
    role: pathParams[0],
    index: pathParams[1],
    roleIdentity: PATH_ROLE_IDENTITY[`role${pathParams[0]}`],
  };
};

// [2147485500, 2147485463, 2147483648] => 1852'/1815'/0'
export const hardenedPathToString = (hardendedPath: Array<string>) => {
  const path = map(hardendedPath, (chunk) => `${chunk - HARDENED}H`);
  return derivationPathToString(path).replace('m/', '');
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
