// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import type { StoresMap } from '../stores/index';
import type { ActionsMap } from '../actions/index';

export type InjectedProps = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
  isICO?: boolean;
};
export type InjectedStoresProps = {
  stores: any | StoresMap;
};
export type InjectedContainerProps = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
  children: Node;
};
export type InjectedDialogContainerProps = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
  children: Node;
  onClose: (...args: Array<any>) => any;
};
export type InjectedDialogContainerFilterProps = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
  children: Node;
  onFilter: (...args: Array<any>) => any;
};
export type InjectedDialogContainerStepProps = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
  children: Node;
  isVideoWatched?: boolean;
  onClose: (...args: Array<any>) => any;
  onContinue: (...args: Array<any>) => any;
  onBack: (...args: Array<any>) => any;
};
export const InjectedDialogContainerStepDefaultProps = {
  actions: null,
  stores: null,
  children: null,
  onClose: () => {},
  onContinue: () => {},
  onBack: () => {},
};
