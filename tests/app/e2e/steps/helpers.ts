import { When } from "cucumber";
import { getProcessesByName } from "../../../../source/main/utils/processes";

const ACTIVE_RESTORE_NOTIFICATION = '.ActiveRestoreNotification';
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
export const getCardanoNodeState = async (client: Record<string, any>) => (await client.execute(() => daedalus.stores.networkStatus.cardanoNodeState)).value;
export const refreshClient = async (client: Record<string, any>) => {
  await client.url(`file://${__dirname}/../../../../dist/renderer/index.html`);
};
const oneHour = 60 * 60 * 1000;
// Helper step to pause execution for up to an hour ;)
When(/^I freeze$/, {
  timeout: oneHour
}, callback => {
  setTimeout(callback, oneHour);
});
// @ts-ignore ts-migrate(2741) FIXME: Property 'isHidden' is missing in type '{}' but re... Remove this comment to see the full error message
export const waitForActiveRestoreNotification = (client: Record<string, any>, {
  isHidden
}: {
  isHidden: boolean;
} = {}) => client.waitForVisible(ACTIVE_RESTORE_NOTIFICATION, null, isHidden);
export const waitForCardanoNodeToExit = async (client: Record<string, any>) => client.waitUntil(async () => (await getProcessesByName('cardano-node')).length === 0, 61000);
export const waitForDaedalusToExit = async (client: Record<string, any>, timeout = 61000) => {
  const daedalusProcessName = process.platform === 'linux' ? 'electron' : 'Electron';
  return client.waitUntil(async () => (await getProcessesByName(daedalusProcessName)).length === 0, timeout);
};