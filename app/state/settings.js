// @flow
import { extendObservable } from 'mobx';
import type { appState } from './index';
import Profile from '../domain/Profile';

export type settingsState = {
  profile: Profile
};

const defaultValues = {
  // TODO: refactor this into api call or something!
  termsOfUse: `
    <h1>Terms of use</h1>
    <p>
      First paragraph quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non 
      numquam eius modi tempora incidunt ut labore et dolore some link aliquam quaerat voluptatem.
    </p>
    <p>
      Second paragraph ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut 
      aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse 
      quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?
    </p>
  `
};

export default (stores): settingsState => (extendObservable(defaultValues, {
  get profile() {
    const user = stores.user.active;
    return user ? user.profile : null;
  }
}));
