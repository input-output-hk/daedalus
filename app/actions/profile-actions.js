// @flow
import { Action } from './lib/actions';

// ======= PROFILE ACTIONS =======

export type ProfileActions = {
  updateLocale: Action<{ locale: string }>,
};

const profileActions: ProfileActions = {
  updateLocale: new Action(),
};

export default profileActions;
