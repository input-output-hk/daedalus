// @flow

const checkCondition = (
  condition: () => boolean,
  resolve: Function,
  reject: Function,
  timeout: number,
  retryEvery: number,
  timeWaited: number = 0
) => {
  if (condition()) {
    resolve();
  } else if (timeWaited >= timeout) {
    reject();
  } else {
    setTimeout(() => checkCondition(
      condition, resolve, reject, timeout, retryEvery, timeWaited + retryEvery
    ), retryEvery);
  }
};

export const promisedCondition = (
  cond: Function, timeout: number = 5000, retryEvery: number = 1000
): Promise<void> => (
  new Promise((resolve, reject) => {
    checkCondition(cond, resolve, reject, timeout, retryEvery);
  })
);
