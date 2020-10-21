// @flow
import { map } from 'lodash';

// Constants
export const CERTIFICATE_TYPE = {
  register_reward_account: 0, // register_reward_account
  quit_pool: 1, // quit_pool
  join_pool: 2, // join_pool
};

// Helpers
export const derivationPathToString = (
  path: Array<string>,
) => {
  let constructedPath = 'm';
  map(path, chunk => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  })
  return constructedPath;
};