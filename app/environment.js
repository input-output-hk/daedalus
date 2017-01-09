// @flow
const environment = Object.assign({
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  WITH_CARDANO_API: 0,
  FAKE_RESPONSE_TIME: 1000,
  AUTO_LOGIN: 0,
  MOBX_DEV_TOOLS: 0,
  current: process.env.NODE_ENV,
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
}, process.env);

export default environment;
