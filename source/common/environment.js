// @flow
import os from 'os';
import { uniq } from 'lodash';
import { version } from '../../package.json';

// Only require electron / remote if we are in a node.js environment
let remote;
if (module && module.require) {
  remote = module.require('electron').remote;
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
  API: process.env.API || 'ada',
  API_VERSION,
  MOBX_DEV_TOOLS: process.env.MOBX_DEV_TOOLS,
  current: process.env.NODE_ENV || 'development',
  REPORT_URL: process.env.REPORT_URL || 'http://report-server.awstest.iohkdev.io:8080/',
  WALLET_PORT: parseInt(process.env.WALLET_PORT || '8090', 10),
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
  isMainnet: () => environment.NETWORK === 'mainnet',
  isAdaApi: () => environment.API === 'ada',
  isEtcApi: () => environment.API === 'etc',
  build,
  buildNumber: uniq([API_VERSION, build]).join('.'),
  getBuildLabel: () => {
    let buildLabel = `Daedalus (${environment.version}#${environment.buildNumber})`;
    if (!environment.isProduction()) buildLabel += ` ${environment.current}`;
    return buildLabel;
  },
  platform,
  os: osNames[platform] || platform,
  version,
}, remote ? remote.getGlobal('env') : process.env);

export default environment;
