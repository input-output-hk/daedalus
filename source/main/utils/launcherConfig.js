import { readFileSync } from 'fs';

const yamljs = require('yamljs');

function getLauncherConfig() {

  if (process.env.LAUNCHER_CONFIG) {
    console.log('Getting Data Directory from Launcher config');
    const inputYaml = readFileSync(process.env.LAUNCHER_CONFIG, 'utf8');
    if (process.env.XDG_DATA_HOME === undefined) {
      process.env.XDG_DATA_HOME = process.env.HOME + '/.local/share/';
    }
    const finalYaml = inputYaml.replace(/\${([^}]+)}/g,
      (a, b) => {
        const res = process.env[b];
        if (res === undefined) {
          console.log('warning var undefined:', b);
          return '';
        }
        return res;
      });
    return yamljs.parse(finalYaml);
  }
  return {};

}
export const launcherConfig = getLauncherConfig();
