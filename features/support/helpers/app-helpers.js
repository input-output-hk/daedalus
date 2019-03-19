// @flow
import type { WebdriverClient } from '../global-types';
import { getProcessesByName } from '../../../source/main/utils/processes';

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

export const refreshClient = async (client: WebdriverClient) => {
  await client.url(`file://${__dirname}/../../../dist/renderer/index.html`);
};
