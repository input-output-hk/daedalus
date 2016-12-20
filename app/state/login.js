// @flow
import { action, extendObservable } from 'mobx';

export type loginState = {
  isLoggingIn: boolean,
  isLoggedIn: boolean,
  isLoading: boolean,
  errorLoading: ?string,
  errorLoggingIn: ?string
};

const defaultValues = {
  isLoggingIn: false,
  isLoggedIn: false,
  isLoading: false,
  errorLoading: null,
  errorLoggingIn: null
};

const state = {};

export default (): loginState => (extendObservable(state, defaultValues, {
  reset: action(() => Object.assign(state, defaultValues))
}));
