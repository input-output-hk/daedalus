// @flow

const environment = {
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  current: process.env.NODE_ENV,
  isDev: () => environment.current === environment.DEVELOPMENT,
  isTest: () => environment.current === environment.TEST,
  isProduction: () => environment.current === environment.PRODUCTION
};

export default environment;
