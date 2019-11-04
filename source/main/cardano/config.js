// @flow
import type { LauncherConfig } from '../config';

const isDev = process.env.NODE_ENV === 'development';

export const ensureXDGDataIsSet = () => {
  if (process.env.HOME && process.env.XDG_DATA_HOME === undefined) {
    process.env.XDG_DATA_HOME = `${process.env.HOME}/.local/share/`;
  }
};

/**
 * Transforms the launcher config to an array of string args
 * which can be passed to the cardano-node process.
 *
 * @param config
 * @returns {NodeArgs}
 * @private
 */
export const prepareArgs = (config: LauncherConfig) => {
  const args: Array<string> = Array.from(config.walletArgs);
  if (config.nodeDbPath) args.push('--db-path', config.nodeDbPath);
  if (config.nodeLogConfig) args.push('--log-config', config.nodeLogConfig);
  if (config.logsPrefix) args.push('--logs-prefix', config.logsPrefix);
  if (config.configuration) {
    if (config.configuration.filePath)
      args.push('--configuration-file', config.configuration.filePath);
    if (config.configuration.key)
      args.push('--configuration-key', config.configuration.key);
    if (config.configuration.systemStart)
      args.push('--system-start', config.configuration.systemStart);
    if (config.configuration.seed)
      args.push('--configuration-seed', config.configuration.seed);
  }
  if (isDev) args.push('--wallet-doc-address', '127.0.0.1:8091');
  return args;
};
