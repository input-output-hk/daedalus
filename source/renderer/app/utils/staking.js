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
    retiringPoolsHidden = true,
    privatePoolsHidden = true,
    poolsWithoutOffChainDataHidden = true,
  } = filterOptions || {};
  let result = 0;

  if (retiringPoolsHidden) {
    result++;
  }
  if (privatePoolsHidden) {
    result++;
  }
  if (poolsWithoutOffChainDataHidden) {
    result++;
  }

  return result;
};

export const isStakePoolInFilterRange = (
  filterOptions: ?StakePoolFilterOptionsType,
  stakePool: StakePool
) => {
  const {
    retiringPoolsHidden = true,
    privatePoolsHidden = true,
    poolsWithoutOffChainDataHidden = true,
  } = filterOptions || {};

  if (
    (stakePool.isRetiring && retiringPoolsHidden) ||
    (stakePool.isPrivate && privatePoolsHidden) ||
    (stakePool.hasNoOffChainData && poolsWithoutOffChainDataHidden)
  ) {
    return false;
  }

  return true;
};
