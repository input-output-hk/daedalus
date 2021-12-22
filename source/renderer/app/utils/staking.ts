// @flow
import { reduce } from 'lodash';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
} from '../config/stakingConfig';
import type { SmashServerType } from '../types/stakingTypes';

export const getSmashServerNameFromUrl = (smashServerUrl: string): string =>
  reduce(
    SMASH_SERVERS_LIST,
    (result, { name, url }) => {
      if (url === smashServerUrl) result = name;
      return result;
    },
    smashServerUrl
  );

export const getSmashServerIdFromUrl = (
  smashServerUrl: string
): SmashServerType =>
  reduce(
    SMASH_SERVERS_LIST,
    (result, { url }, id) => {
      if (url === smashServerUrl) result = id;
      return result;
    },
    SMASH_SERVER_TYPES.CUSTOM
  );

export const getUrlParts = (
  url: string
): {
  hash: string,
  host: string,
  hostname: string,
  href: string,
  origin: string,
  password: string,
  pathname: string,
  port: string,
  protocol: string,
  search: string,
  searchParams: URLSearchParams,
  username: string,
} => {
  try {
    return new URL(url);
  } catch (error) {
    return {};
  }
};
