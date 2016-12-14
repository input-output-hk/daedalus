// @flow
import { action } from 'mobx';

export type loginState = {
  isLoggingIn: boolean,
  isLoggedIn: boolean,
  isLoading: boolean,
  errorLoading: ?string
};

const defaultValues = {
  isLoggingIn: false,
  isLoggedIn: false,
  isLoading: false,
  errorLoading: null
};

const state = {};

export default (): loginState => (Object.assign(state, defaultValues, {
  reset: action(() => Object.assign(state, defaultValues))
}));
