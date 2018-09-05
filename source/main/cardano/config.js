// @flow
import { readFileSync } from 'fs';
import yamljs from 'yamljs';

/**
 * The shape of the config params, usually provided to the cadano-node launcher
 */
export type LauncherConfig = {
  nodePath: string,
  nodeArgs: Array<string>,
  reportServer?: string,
  nodeDbPath?: string,
  logsPrefix?: string,
  nodeTimeoutSec: number,
  configuration?: {
    filePath?: string,
    key?: string,
    systemStart?: string,
    seed?: string,
  }
};

export const ensureXDGDataIsSet = () => {
  if (process.env.HOME && process.env.XDG_DATA_HOME === undefined) {
    process.env.XDG_DATA_HOME = process.env.HOME + '/.local/share/';
  }
};

export const readLauncherConfig = (configFile: string) => {
  const inputYaml = readFileSync(configFile, 'utf8');
  const finalYaml = inputYaml.replace(/\${([^}]+)}/g,
    (a, b) => {
      if (process.env[b]) {
        return process.env[b];
      }
      console.log('readLauncherConfig: warning var undefined:', b);
      return '';
    }
  );
  return yamljs.parse(finalYaml);
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
  const args: Array<string> = Array.from(config.nodeArgs);
  if (config.reportServer) args.push('--report-server', config.reportServer);
  if (config.nodeDbPath) args.push('--db-path', config.nodeDbPath);
  if (config.logsPrefix) args.push('--logs-prefix', config.logsPrefix);
  if (config.configuration) {
    if (config.configuration.filePath) args.push('--configuration-file', config.configuration.filePath);
    if (config.configuration.key) args.push('--configuration-key', config.configuration.key);
    if (config.configuration.systemStart) args.push('--system-start', config.configuration.systemStart);
    if (config.configuration.seed) args.push('--configuration-seed', config.configuration.seed);
  }
  return args;
};
