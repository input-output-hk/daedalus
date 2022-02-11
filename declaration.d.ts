import { Environment } from './source/common/types/environment.types';

declare module '*.svg' {
  const content: any;
  export default content;
}

declare module '*.scss' {
  const content: any;
  export default content;
}

type Daedalus = {
  actions: ActionsMap;
  api: Api;
  environment: Object;
  reset: Function;
  stores: StoresMap;
  translations: Object;
  utils: {
    crypto: {
      generateMnemonic: Function;
    };
  };
};

export type $ElementType<
  T extends { [P in K & any]: any },
  K extends keyof T | number
> = T[K];

export type EnumMap<
  K extends string,
  V,
  O extends Record<string, any> = any
> = O & Record<K, V & $ElementType<O, K>>;

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      WALLET_COUNT: number;
    }
  }

  var environment: Environment;

  var daedalus: Daedalus;
}

export {};
