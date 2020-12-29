// @flow
import { reduce } from 'lodash';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
} from '../config/stakingConfig';

export const getSmashServerNameFromUrl = (smashServerUrl: string) =>
  reduce(
    SMASH_SERVERS_LIST,
    (result, { name, url }) => {
      if (url === smashServerUrl) result = name;
      return result;
    },
    smashServerUrl
  );

export const getSmashServerIdFromUrl = (smashServerUrl: string) =>
  reduce(
    SMASH_SERVERS_LIST,
    (result, { url }, id) => {
      if (url === smashServerUrl) result = id;
      return result;
    },
    SMASH_SERVER_TYPES.CUSTOM
  );
