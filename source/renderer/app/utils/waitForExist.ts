// @flow

type Options = {
  rejectTimeoutTime?: number,
  checkIntervalTime?: number,
  context?: HTMLElement,
  selectAll?: boolean,
};

const REJECT_TIMEOUT = 10000;
const CHECK_INTERVAL = 500;

export const waitForExist = (
  selector: string,
  options?: Options = {}
): Promise<any> => {
  const {
    rejectTimeoutTime = REJECT_TIMEOUT,
    checkIntervalTime = CHECK_INTERVAL,
    context = document,
    selectAll,
  } = options;
  return new Promise((resolve, reject) => {
    const rejectTimeout = setTimeout(() => {
      clearInterval(checkInterval);
      reject(new Error('Element(s) not found'));
    }, rejectTimeoutTime);

    const doResolveSingle = (selection: HTMLElement) => {
      resolve(selection);
      clearInterval(checkInterval);
      clearTimeout(rejectTimeout);
    };

    const doResolveAll = (selection: NodeList<HTMLElement>) => {
      resolve(selection);
      clearInterval(checkInterval);
      clearTimeout(rejectTimeout);
    };

    const checkAll = () => {
      const selection = context.querySelectorAll(selector);
      if (selection.length) doResolveAll(selection);
    };

    const checkSingle = () => {
      const selection = context.querySelector(selector);
      if (selection) doResolveSingle(selection);
    };

    const check = selectAll ? checkAll : checkSingle;

    const checkInterval = setInterval(check, checkIntervalTime);
    check();
  });
};
