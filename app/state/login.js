// @flow
import { action } from 'mobx';

export type loginState = {
  isLoading: boolean,
  errorLoading: ?string
};

const defaultValues = {
  isLoading: false,
  errorLoading: null
};

const state = {};

export default (): loginState => (Object.assign(state, defaultValues, {
  reset: action(() => Object.assign(state, defaultValues))
}));
