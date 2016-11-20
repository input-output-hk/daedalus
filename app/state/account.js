// @flow
import type { appState } from './index';
import UserAccount from '../domain/UserAccount';

export type accountState = {
  userAccount: UserAccount,
  isLoading: boolean,
  isUpdatingName: boolean,
  errorLoading: ?string,
  errorUpdatingName: ?string
};

export default (state: appState): accountState => ({
  state,
  userAccount: new UserAccount(),
  isLoading: false,
  isUpdatingName: false,
  errorLoading: null,
  errorUpdatingName: null
});
