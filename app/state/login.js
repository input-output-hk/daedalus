// @flow
import type { appState } from './index';

export type loginState = {
  isLoading: boolean,
  errorLoading: ?string
};

export default (state: appState): loginState => ({
  state,
  isLoading: false,
  errorLoading: null
});
