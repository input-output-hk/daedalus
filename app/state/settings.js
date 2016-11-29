// @flow
import type { appState } from './index';
import Profile from '../domain/Profile';

export type settingsState = {
  profile: () => Profile
};

export default (state: appState): settingsState => ({
  profile: () => state.user.profile
});
