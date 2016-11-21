// @flow
import type { appState } from './index';
import Profile from '../domain/Profile';

export type settingsState = {
  state: appState,
  profile: () => Profile
};

export default (state: appState): settingsState => ({
  state,
  profile: () => state.user.profile
});
