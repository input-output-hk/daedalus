// @flow
import { map, join } from 'lodash';
import { cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';

// Constants
export const CERTIFICATE_TYPE = {
  register_reward_account: 0, // register_reward_account
  quit_pool: 1, // quit_pool
  join_pool: 2, // join_pool
};

// Helpers
export const derivationPathToString = (derivationPath: Array<string>) => {
  let constructedPath = 'm';
  map(derivationPath, (chunk) => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  });
  return constructedPath;
};

export const derivationPathToLedgerPath = (derivationPath: Array<string>) => {
  const transformedPath = map(derivationPath, (chunk) => chunk.replace('H', "'"));
  const constructedPath = join(transformedPath, '/');
  return cardano.str_to_path(constructedPath);
};

