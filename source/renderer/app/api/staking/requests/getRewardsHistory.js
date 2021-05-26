// @flow
import { request } from 'graphql-request';
import { print } from 'graphql/language/printer';
import getRewardsForAddressesQuery from './graphql/getRewardsForAddresses.graphql';
import type {
  GetRewardsForAddressesQuery,
  GetRewardsForAddressesQueryVariables,
} from '../../../types/cardano-graphql';

export async function getRewardsHistory(
  vars: GetRewardsForAddressesQueryVariables
): Promise<GetRewardsForAddressesQuery> {
  return request(
    global.environment.cardanoGraphQlEndpoint,
    print(getRewardsForAddressesQuery),
    vars
  );
}
