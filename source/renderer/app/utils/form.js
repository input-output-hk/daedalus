// @flow

export const submitOnEnter = (
  callback: Function,
  event: KeyboardEvent
): void => {
  if (event.target instanceof HTMLInputElement && event.key === 'Enter')
    callback();
};
