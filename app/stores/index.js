import { observable } from 'mobx';
import AppStore from './AppStore';

// Constant that does never change during lifetime
const stores = observable({
  app: null,
});

// Set up and return the stores for this app
// can also be used to reset all stores to defaults
export default (api) => (
  Object.assign(stores, {
    app: new AppStore(stores, api),
  })
);

export type storesType = {
  app: AppStore
};
