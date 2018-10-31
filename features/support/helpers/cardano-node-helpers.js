// @flow
import type {
  Daedalus,
  WebdriverClient
} from '../global-types';
import { getProcessesByName } from '../../../source/main/utils/processes';

declare var daedalus: Daedalus;

export const getCardanoNodeState = async (client: WebdriverClient) => (
  (await client.execute(() => daedalus.stores.networkStatus.cardanoNodeState)).value
);

export const waitForCardanoNodeToExit = async (client: WebdriverClient) => (
  await client.waitUntil(async () => (
    (await getProcessesByName('cardano-node')).length === 0
  ), 61000)
)
