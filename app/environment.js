// @flow
const environment = Object.assign({
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  CARDANO_API,
  FAKE_RESPONSE_TIME,
  AUTO_LOGIN,
  MOBX_DEV_TOOLS,
  DAEDALUS_VERSION,
  current: process.env.NODE_ENV,
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
}, process.env);

export default environment;
