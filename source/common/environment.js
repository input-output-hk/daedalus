// @flow
const environment = Object.assign({
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  NETWORK: process.env.NETWORK,
  API: process.env.API || 'ada',
  MOBX_DEV_TOOLS: process.env.MOBX_DEV_TOOLS,
  current: process.env.NODE_ENV,
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
  isMainnet: () => environment.NETWORK === 'mainnet',
  isAdaApi: () => environment.API === 'ada',
  isEtcApi: () => environment.API === 'etc',
}, process.env);

export default environment;
