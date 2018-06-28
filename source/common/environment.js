// @flow
import os from 'os';
import { version } from '../../package.json';

// Only require electron / remote if we are in a node.js environment
let remote;
if (module && module.require) {
  remote = module.require('electron').remote;
}

const environment = Object.assign({
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  NETWORK: process.env.NETWORK,
  API: process.env.API || 'ada',
  API_VERSION: process.env.API_VERSION || 'dev',
  MOBX_DEV_TOOLS: process.env.MOBX_DEV_TOOLS,
  current: process.env.NODE_ENV,
  REPORT_URL: process.env.REPORT_URL || 'http://report-server.awstest.iohkdev.io:8080/',
  WALLET_PORT: parseInt(process.env.WALLET_PORT || '8090', 10),
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
  isMainnet: () => environment.NETWORK === 'mainnet',
  isAdaApi: () => environment.API === 'ada',
  isEtcApi: () => environment.API === 'etc',
  build: process.env.BUILD_NUMBER || 'dev',
  platform: os.platform(),
  version,
}, remote ? remote.getGlobal('env') : process.env);

export default environment;
