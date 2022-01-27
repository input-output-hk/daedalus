import { defineParameterType } from "cucumber";
import type { Api } from "../source/renderer/app/api";
import type { ActionsMap } from "../source/renderer/app/actions";
import type { StoresMap } from "../source/renderer/app/stores";
// Add {bool} parameter type
defineParameterType({
  name: 'bool',
  regexp: /true|false/,
  transformer: b => b === 'true'
});
export type Daedalus = {
  actions: ActionsMap;
  api: Api;
  environment: Record<string, any>;
  reset: (...args: Array<any>) => any;
  stores: StoresMap;
  translations: Record<string, any>;
  utils: {
    crypto: {
      generateMnemonic: (...args: Array<any>) => any;
    };
  };
};
export type WebdriverExecuteResult<T> = {
  value: T;
};
export type WebdriverClient = {
  click: (selector: string) => Promise<any>;
  elements: (selector: string) => Promise<Record<string, any>>;
  execute: (script: (...args: Array<any>) => any, ...scriptArgs: Array<any>) => WebdriverExecuteResult<any>;
  executeAsync: (script: (...args: Array<any>) => any, ...scriptArgs: Array<any>) => Promise<WebdriverExecuteResult<any>>;
  getText: (selector: string) => Promise<any>;
  url: (url: string) => Promise<any>;
  waitForEnabled: (selector: string, ms?: number | null, reverse?: boolean) => Promise<any>;
  waitForText: (selector: string) => Promise<any>;
  waitForVisible: (target: string, ms?: number | null, reverse?: boolean) => Promise<any>;
  waitUntil: (script: (...args: Array<any>) => any, timeout?: number) => Promise<any>;
};