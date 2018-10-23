// @flow
import type {
  Daedalus,
  WebdriverClient
} from '../global-types';

declare var daedalus: Daedalus;

export const getCardanoNodeState = async (client: WebdriverClient) => (
  (await client.execute(() => daedalus.stores.networkStatus.cardanoNodeState)).value
);
