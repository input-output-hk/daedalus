import { readFileSync } from 'fs';
import log from 'electron-log';

const yamljs = require('yamljs');

function getLauncherConfig() {
  if (process.env.LAUNCHER_CONFIG) {
    log.info('Getting Data Directory from Launcher config');
    const inputYaml = readFileSync(process.env.LAUNCHER_CONFIG, 'utf8');

    // Linux usage refers to $XDG_DATA_HOME which needs a default value when not set
    if (process.env.XDG_DATA_HOME === undefined) {
      process.env.XDG_DATA_HOME = process.env.HOME + '/.local/share/';
    }

    const finalYaml = inputYaml.replace(/\${([^}]+)}/g,
      (a, b) => {
        const res = process.env[b];
        if (res === undefined) {
          log.info('Warning var undefined:', b);
          return '';
        }
        return res;
      });
    return yamljs.parse(finalYaml);
  }
  return {};
}

export const launcherConfig = getLauncherConfig();
