import Store from 'electron-store';
import { logger } from './logging';
const store = new Store();

const getStoreKey = (network: string): string => `${network}-RTS-FLAGS`;

export const getRtsFlagsSettings = (network: string): string[] | null => {
  try {
    const flags = store.get(getStoreKey(network));
    logger.info(`[RTS-FLAGS] Read ${network} flags: ${flags} from config`);
    return flags;
  } catch (error) {
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
  logger.info(
    `[RTS-FLAGS] Persisted ${network} flags: [${flags.toString()}] in config`
  );
  store.set(getStoreKey(network), flags);
};
