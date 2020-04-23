// @flow
import { When } from 'cucumber';
import { getProcessesByName } from '../../../../source/main/utils/processes';
import type { Daedalus, WebdriverClient } from '../../../types';

declare var daedalus: Daedalus;

const ACTIVE_RESTORE_NOTIFICATION = '.ActiveRestoreNotification';

export const getCardanoNodeState = async (client: Object) =>
  (await client.execute(() => daedalus.stores.networkStatus.cardanoNodeState)).value;

export const refreshClient = async (client: Object) => {
  await client.url(`file://${__dirname}/../../../../dist/renderer/index.html`);
};

const oneHour = 60 * 60 * 1000;
// Helper step to pause execution for up to an hour ;)
When(/^I freeze$/, { timeout: oneHour }, callback => {
  setTimeout(callback, oneHour);
});

export const waitForActiveRestoreNotification = (
  client: Object,
  { isHidden } : { isHidden: boolean } = {}
) =>
  client.waitForVisible(
    ACTIVE_RESTORE_NOTIFICATION,
    null,
    isHidden
  );

export const waitForCardanoNodeToExit = async (client: Object) =>
  client.waitUntil(
    async () => (await getProcessesByName('cardano-node')).length === 0,
    61000
  );

export const waitForDaedalusToExit = async (
  client: Object,
  timeout: number = 61000
) => {
  const daedalusProcessName =
    process.platform === 'linux' ? 'electron' : 'Electron';
  return client.waitUntil(
    async () => (await getProcessesByName(daedalusProcessName)).length === 0,
    timeout
  );
};
