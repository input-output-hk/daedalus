import Store from 'electron-store';
import { logger } from './logging';

const store = new Store();

const getStoreKey = (network: string): string => `${network}-RTS-FLAGS`;

export const getRtsFlagsSettings = (network: string): string[] | null => {
  try {
    const flags = store.get(getStoreKey(network));
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info(`[RTS-FLAGS] Read ${network} flags: ${flags} from config`);
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    return flags;
  } catch (error) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.error(
      `[RTS-FLAGS] Failed to read ${network} flags from config`,
      error
    );
  }

  return null;
};
export const storeRtsFlagsSettings = (
  network: string,
  flags: string[]
): void => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logger.info(
    `[RTS-FLAGS] Persisted ${network} flags: [${flags.toString()}] in config`
  );
  store.set(getStoreKey(network), flags);
};
