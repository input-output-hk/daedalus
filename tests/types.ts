// @flow
import { defineParameterType } from 'cucumber';
import type { Api } from '../source/renderer/app/api';
import type { ActionsMap } from '../source/renderer/app/actions';
import type { StoresMap } from '../source/renderer/app/stores';

// Add {bool} parameter type
defineParameterType({
  name: 'bool',
  regexp: /true|false/,
  transformer: b => b === 'true',
});

export type Daedalus = {
  actions: ActionsMap,
  api: Api,
  environment: Object,
  reset: Function,
  stores: StoresMap,
  translations: Object,
  utils: {
    crypto: {
      generateMnemonic: Function
    }
  },
};

export type WebdriverExecuteResult<T> = { value: T };

export type WebdriverClient = {
  click: (selector: string) => Promise<any>,
  elements: (selector: string) => Promise<Object>,
  execute: (script: Function, ...scriptArgs: Array<any>) => WebdriverExecuteResult<any>,
  executeAsync: (script: Function, ...scriptArgs: Array<any>) => Promise<WebdriverExecuteResult<any>>,
  getText: (selector: string) => Promise<any>,
  url: (url: string) => Promise<any>,
  waitForEnabled: (selector: string, ms?: number | null, reverse?: boolean) => Promise<any>,
  waitForText: (selector: string) => Promise<any>,
  waitForVisible: (target: string, ms?: number | null, reverse?: boolean) => Promise<any>,
  waitUntil: (script: Function, timeout?: number) => Promise<any>,
};
