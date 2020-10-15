// @flow

export const waitFor = (conditionFunction: Function): Promise<any> => {
  const poll = (resolve) => {
    if (conditionFunction()) resolve();
    else setTimeout(() => poll(resolve), 400);
  };
  return new Promise(poll);
};
