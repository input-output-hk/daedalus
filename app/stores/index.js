// @flow
import { observable } from 'mobx';
import AppStore from './AppStore';
import UserStore from './UserStore';
import SettingsStore from './SettingsStore';

// Constant that does never change during lifetime
const stores = observable({
  app: null,
  user: null,
  settings: null,
});

// Set up and return the stores for this app
// can also be used to reset all stores to defaults
export default (api, actions) => (
  Object.assign(stores, {
    app: new AppStore(stores, api, actions),
    user: new UserStore(stores, api, actions),
    settings: new SettingsStore(stores, api, actions),
  })
);

export type storesType = {
  app: AppStore,
  user: UserStore,
  settings: SettingsStore,
};
