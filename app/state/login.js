// @flow
export type loginState = {
  isLoading: boolean,
  errorLoading: ?string
};

export default (): loginState => ({
  isLoading: false,
  errorLoading: null
});
