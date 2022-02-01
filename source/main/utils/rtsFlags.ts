import Store from 'electron-store';
import { logger } from './logging';
import { relaunch } from './safeExitWithCode';

const store = new Store();
export const getRtsFlags = (network: string): string[] | null => {
  try {
    const rtsFlags = store.get(`${network}-RTS-FLAGS`);
    logger.info(`[GET-RTS-FLAGS] ${network}-RTS-FLAGS: `, rtsFlags);
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    return rtsFlags;
  } catch (error) {
    logger.error(`[GET-RTS-FLAGS] Error fetching ${network}-RTS-FLAGS`, error);
  }

  return null;
};
export const setRtsFlagsAndRestart = (
  network: string,
  flags: Array<string>
): void => {
  logger.info('[SET-RTS-FLAGS] setting: ', {
    network,
    flags,
  });
  store.set(`${network}-RTS-FLAGS`, flags);
  relaunch();
};
