// @flow
import { Action } from './lib/actions';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  updateLocale: Action<{ locale: string }> = new Action();
}
