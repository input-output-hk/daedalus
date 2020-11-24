// @flow
import { map, join, takeRight } from 'lodash';
import { cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { HARDENED } from '../config/hardwareWalletsConfig';

// Constants
export const CERTIFICATE_TYPE = {
  register_reward_account: 0, // register_reward_account
  quit_pool: 1, // quit_pool
  join_pool: 2, // join_pool
};

export const PATH_ROLE_IDENTITY = {
  0: 'utxo_external', // address
  1: 'utxo_internal', // change
  2: 'mutable_account', // stake
  3: 'multisig_script', // script
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
  return cardano.str_to_path(constructedPath);
};

export const getParamsFromPath = (derivationPath: Array<string>) => {
  const pathParams = takeRight(derivationPath, 2);
  return {
    role: pathParams[0],
    index: pathParams[1],
    roleIdentity: PATH_ROLE_IDENTITY[pathParams[0]],
  };
};

// [2147485500, 2147485463, 2147483648] => 1852'/1815'/0'
export const hardenedPathToString = (hardendedPath: Array<string>) => {
  const path = map(hardendedPath, (chunk) => `${chunk - HARDENED}H`);
  return derivationPathToString(path).replace('m/', '');
};
