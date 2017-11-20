// @flow
import type { Node } from 'react';
import type { StoresMap } from '../stores/index';
import type { ActionsMap } from '../actions/index';

export type InjectedProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

export type InjectedContainerProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  children: Node,
};

export type InjectedDialogContainerProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  children: Node,
  onClose: Function,
};
