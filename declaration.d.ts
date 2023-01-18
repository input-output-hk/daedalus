import { IpcRenderer } from 'electron';
import type { request as httpRequest } from 'http';
import type { request as httpsRequest, globalAgent } from 'https';
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

interface Http {
  request: httpRequest;
}

interface Https {
  request: httpsRequest;
  Agent: globalAgent;
}

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
  /* eslint-disable no-var, vars-on-top */
  var daedalus: Daedalus;
  var environment: Environment;
  var http: Http;
  var https: Https;
  var legacyStateDir: string;
  var isFlight: boolean;
  var ipcRenderer: Pick<
    IpcRenderer,
    'on' | 'once' | 'send' | 'removeListener' | 'removeAllListeners'
  >;
  /* eslint-enable no-var, vars-on-top */
}

export {};
