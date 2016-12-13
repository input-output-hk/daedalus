// @flow
import { action } from 'mobx';

export type loginState = {
  isLoggedIn: boolean,
  isLoading: boolean,
  errorLoading: ?string
};

const defaultValues = {
  isLoggedIn: false,
  isLoading: false,
  errorLoading: null
};

const state = {};

export default (): loginState => (Object.assign(state, defaultValues, {
  reset: action(() => Object.assign(state, defaultValues))
}));
