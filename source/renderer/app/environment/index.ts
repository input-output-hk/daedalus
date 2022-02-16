import { createEnvironment } from 'react-states';
import { LocalStorage } from './localstorage';
import type { Environment as Config } from '../../../common/types/environment.types';

export interface Environment {
  storage: LocalStorage;
  config: Config;
}

const { EnvironmentProvider, useEnvironment } = createEnvironment<
  Environment
>();

export { EnvironmentProvider, useEnvironment };
