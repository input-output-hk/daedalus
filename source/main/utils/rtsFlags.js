// @flow
import Store from 'electron-store';
import { logger } from './logging';
import { relaunch } from './safeExitWithCode';

const store = new Store();

export const getRtsFlags = (network: string): string[] => {
  try {
    const rtsFlags = store.get(`${network}-RTS-FLAGS`);
    logger.info(`[GET-RTS-FLAGS] ${network}-RTS-FLAGS: `, rtsFlags);
    return rtsFlags;
  } catch (error) {
    logger.error(`[GET-RTS-FLAGS] Error fetching ${network}-RTS-FLAGS`, error);
  }
  return [];
};

export const setRtsFlagsAndRestart = (
  network: string,
  flags: Array<string>
): void => {
  logger.info('[SET-RTS-FLAGS] setting: ', { network, flags });
  store.set(`${network}-RTS-FLAGS`, flags);
  relaunch();
};
