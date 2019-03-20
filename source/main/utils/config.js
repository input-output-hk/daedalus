// @flow
import { readFileSync } from 'fs';
import yamljs from 'yamljs';
import type { LauncherConfig } from '../config';

/**
 * Reads and parses the launcher config yaml file on given path.
 * @param configPath {String}
 * @returns {LauncherConfig}
 */
export const readLauncherConfig = (configPath: ?string): LauncherConfig => {
  const inputYaml = configPath ? readFileSync(configPath, 'utf8') : '';
  const finalYaml = inputYaml.replace(/\${([^}]+)}/g, (a, b) => {
    if (process.env[b]) {
      return process.env[b];
    }
    console.log('readLauncherConfig: warning var undefined:', b);
    return '';
  });
  return yamljs.parse(finalYaml);
};
