// @flow
import Action from './lib/Action';

// ======= PROFILE ACTIONS =======

export default class ProfileActions {
  updateLocale: Action<{ locale: string }> = new Action();
}
