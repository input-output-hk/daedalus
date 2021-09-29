// @flow

export const submitOnEnter = (callback, event: KeyboardEvent): void => {
  if (event.target instanceof HTMLInputElement && event.key === 'Enter')
    callback();
};
