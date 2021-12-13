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

//declare type EnumMap<K: string, V, O: Object = *> = O & { [K]: V & <O, K> };

declare global {
  namespace NodeJS {
    interface ProcessEnv {
      WALLET_COUNT: number;
    }
  }
  var daedalus: Daedalus;
}

export {};
