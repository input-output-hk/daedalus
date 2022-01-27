import { readFileSync } from 'fs';
import yamljs from 'yamljs';
import type { LauncherConfig } from '../config';

function recurseReplace(obj) {
  if (Array.isArray(obj)) {
    const out = [];

    for (let idx in obj) {
      if (Object.prototype.hasOwnProperty.call(obj, idx)) {
        // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'string'.
        idx = parseInt(idx, 10);
        out[idx] = recurseReplace(obj[idx]);
      }
    }

    return out;
  }

  if (obj === null) return null;

  switch (typeof obj) {
    case 'string': {
      return obj.replace(/\${([^}]+)}/g, (a, b) => {
        if (process.env[b]) {
          return process.env[b];
        }

        // eslint-disable-next-line no-console
        console.log('readLauncherConfig: warning var undefined:', b);
        return '';
      });
    }

    case 'object': {
      const out = {};

      for (const key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key)) {
          out[key] = recurseReplace(obj[key]);
        }
      }

      return out;
    }

    default:
      return obj;
  }
}

/**
 * Reads and parses the launcher config yaml file on given path.
 * @param configPath {String}
 * @returns {LauncherConfig}
 */
export const readLauncherConfig = (
  configPath: string | null | undefined
): LauncherConfig => {
  const inputYaml = configPath ? readFileSync(configPath, 'utf8') : '';
  const parsed = yamljs.parse(inputYaml);
  const finalYaml = recurseReplace(parsed);

  if (finalYaml === null || finalYaml === []) {
    throw new Error('Daedalus requires a valid launcher config file to work');
  }

  // @ts-ignore
  return finalYaml;
};
