// @flow
import type { Children } from 'react';
import type { StoresMap } from '../stores/index';
import type { ActionsMap } from '../actions/index';

export type InjectedProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

export type InjectedContainerProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  children: Children,
};
