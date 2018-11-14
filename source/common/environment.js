// @flow
import os from 'os';
import { uniq, upperFirst } from 'lodash';
import { version } from '../../package.json';

// Only require electron / remote if we are in a node.js environment
let remote;
if (process.version !== '' && process.release !== undefined && process.release.name === 'node') {
  remote = require('electron').remote;
}

const osNames = {
  darwin: 'macOS',
  win32: 'Windows',
  linux: 'Linux',
};

const platform = os.platform();
const build = process.env.BUILD_NUMBER || 'dev';
const API_VERSION = process.env.API_VERSION || 'dev';

const environment = Object.assign({
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  NETWORK: process.env.NETWORK || 'development',
  API_VERSION,
  MOBX_DEV_TOOLS: process.env.MOBX_DEV_TOOLS,
  current: process.env.NODE_ENV || 'development',
  REPORT_URL: process.env.REPORT_URL || 'http://staging-report-server.awstest.iohkdev.io:8080/',
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
  isMainnet: () => environment.NETWORK === 'mainnet',
  isStaging: () => environment.NETWORK === 'staging',
  isTestnet: () => environment.NETWORK === 'testnet',
  isDevelopment: () => environment.NETWORK === 'development',
  build,
  buildNumber: uniq([API_VERSION, build]).join('.'),
  getBuildLabel: () => {
    const networkLabel = !(environment.isMainnet() || environment.isDevelopment()) ?
      ` ${upperFirst(environment.NETWORK)}` : '';
    let buildLabel = `Daedalus${networkLabel} (${environment.version}#${environment.buildNumber})`;
    if (!environment.isProduction()) buildLabel += ` ${environment.current}`;
    return buildLabel;
  },
  platform,
  os: osNames[platform] || platform,
  getInstallerVersion: () => uniq([environment.API_VERSION, environment.build]).join('.'),
  version,
  isWindows: () => environment.platform === 'win32'
}, remote ? remote.getGlobal('env') : process.env);

export default environment;
