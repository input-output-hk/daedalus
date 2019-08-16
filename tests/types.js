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
  api: Api,
  environment: Object,
  actions: ActionsMap,
  stores: StoresMap,
  translations: Object,
  reset: Function,
};

export type WebdriverExecuteResult<T> = { value: T };

export type WebdriverClient = {
  execute: (script: Function, scriptArgs: any) => WebdriverExecuteResult<any>,
  url: (url: string) => Promise<any>,
  waitForVisible: (target: string, ms?: number | null, reverse?: boolean) => Promise<any>,
  waitUntil: (script: Function, timeout?: number) => Promise<any>,
};
