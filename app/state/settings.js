// @flow
import { extendObservable } from 'mobx';
import type { appState } from './index';
import Profile from '../domain/Profile';

export type settingsState = {
  profile: Profile
};

const defaultValues = {};

export default (state: appState): settingsState => (extendObservable(defaultValues, {
  get profile() {
    return state.user.profile;
  }
}));
