import { Environment } from '../common/types/environment.types';

declare module '*.scss';
declare module '*.inline.svg';
declare namespace globalThis {
  /* eslint-disable-next-line */
  var isFlight: boolean;
  /* eslint-disable-next-line */
  var environment: Environment;
}
