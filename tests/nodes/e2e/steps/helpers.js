// @flow
import { getProcessesByName } from '../../../../source/main/utils/processes';
import type { Daedalus, WebdriverClient } from '../../../types';

declare var daedalus: Daedalus;

const ACTIVE_RESTORE_NOTIFICATION = '.ActiveRestoreNotification';

export const getCardanoNodeState = async (client: WebdriverClient) =>
  (await client.execute(() => daedalus.stores.networkStatus.cardanoNodeState)).value;

export const refreshClient = async (client: WebdriverClient) => {
  await client.url(`file://${__dirname}/../../../../dist/renderer/index.html`);
};

export const waitForActiveRestoreNotification = (
  client: WebdriverClient,
  { isHidden } : { isHidden: boolean } = {}
) =>
  client.waitForVisible(
    ACTIVE_RESTORE_NOTIFICATION,
    null,
    isHidden
  );

export const waitForCardanoNodeToExit = async (client: WebdriverClient) =>
  client.waitUntil(
    async () => (await getProcessesByName('cardano-node')).length === 0,
    61000
  );

export const waitForDaedalusToExit = async (
  client: WebdriverClient,
  timeout: number = 61000
) => {
  const daedalusProcessName =
    process.platform === 'linux' ? 'electron' : 'Electron';
  return client.waitUntil(
    async () => (await getProcessesByName(daedalusProcessName)).length === 0,
    timeout
  );
};
