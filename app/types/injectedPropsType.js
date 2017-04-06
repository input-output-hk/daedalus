// @flow
import type { Children } from 'react';
import type { StoresMap } from '../stores/index';
import type { ActionsMap } from '../actions/index';

export type InjectedProps = {
  stores: StoresMap,
  actions: ActionsMap,
};

export type InjectedContainerProps = {
  stores: StoresMap,
  actions: ActionsMap,
  children: Children,
};
