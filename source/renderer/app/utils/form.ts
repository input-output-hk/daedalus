export const submitOnEnter = (
  callback: (...args: Array<any>) => any,
  event: KeyboardEvent
): void => {
  if (event.target instanceof HTMLInputElement && event.key === 'Enter')
    callback();
};
