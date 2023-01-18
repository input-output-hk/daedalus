import { MobXProviderContext } from 'mobx-react';
import React from 'react';
import { ActionsMap } from '../actions';

export function useActions(): ActionsMap {
  return React.useContext(MobXProviderContext).actions;
}
