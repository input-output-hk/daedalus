// @flow

const environment = Object.assign({}, {
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  WITH_CARDANO_API: false,
  current: process.env.NODE_ENV,
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION,
}, process.env);

export default environment;
