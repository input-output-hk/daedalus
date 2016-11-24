export default {
  DEVELOPMENT: 'development',
  TEST: 'test',
  PRODUCTION: 'production',
  current: process.env.NODE_ENV,
  isDev: () => this.current === this.DEVELOPMENT,
  isTest: () => this.current === this.TEST
};
