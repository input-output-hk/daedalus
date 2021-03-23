// @flow
import { reduce } from 'lodash';
import {
  SMASH_SERVERS_LIST,
  SMASH_SERVER_TYPES,
} from '../config/stakingConfig';
import type { SmashServerType } from '../types/stakingTypes';
import type { StakePoolFilterOptionsType } from '../stores/StakingStore';
import StakePool from '../domains/StakePool';

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

export const getNumberOfFilterDimensionsApplied = (
  filterOptions: ?StakePoolFilterOptionsType
) => {
  const {
    retiringPoolsChecked = true,
    privatePoolsChecked = true,
    poolsWithoutOffChainDataChecked = true,
  } = filterOptions || {};
  let result = 0;

  if (!retiringPoolsChecked) {
    result++;
  }
  if (!privatePoolsChecked) {
    result++;
  }
  if (!poolsWithoutOffChainDataChecked) {
    result++;
  }

  return result;
};

export const isStakePoolInFilterRange = (
  filterOptions: ?StakePoolFilterOptionsType,
  stakePool: StakePool
) => {
  const {
    retiringPoolsChecked,
    privatePoolsChecked,
    poolsWithoutOffChainDataChecked,
  } = filterOptions;

  if (stakePool.isRetiring && !retiringPoolsChecked) {
    return false;
  }
  if (stakePool.isPrivate && !privatePoolsChecked) {
    return false;
  }
  if (stakePool.hasNoOffChainData && !poolsWithoutOffChainDataChecked) {
    return false;
  }
  return true;
};
