// @flow
import type { Node } from 'react';
import type { StoresMap } from '../stores/index';
import type { ActionsMap } from '../actions/index';

export type InjectedProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

export type InjectedStoresProps = {
  stores: any | StoresMap,
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

export type InjectedDialogContainerStepProps = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  children: Node,
  isVideoWatched?: boolean,
  onClose: Function,
  onContinue: Function,
  onBack: Function,
};

export const InjectedDialogContainerStepDefaultProps = {
  actions: null,
  stores: null,
  children: null,
  isVideoWatch: false,
  onClose: () => {},
  onContinue: () => {},
  onBack: () => {},
};
