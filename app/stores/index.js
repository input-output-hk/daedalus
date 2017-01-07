// @flow
import { observable } from 'mobx';
import AppStore from './AppStore';
import UserStore from './UserStore';

// Constant that does never change during lifetime
const stores = observable({
  app: null,
  user: null,
});

// Set up and return the stores for this app
// can also be used to reset all stores to defaults
export default (api, actions) => (
  Object.assign(stores, {
    app: new AppStore(stores, api, actions),
    user: new UserStore(stores, api, actions),
  })
);

export type storesType = {
  app: AppStore,
  user: UserStore,
};
