import { MobXProviderContext } from 'mobx-react';
import React from 'react';
import { StoresMap } from '../stores';

export function useStores(): StoresMap {
  return React.useContext(MobXProviderContext).stores;
}
