// @flow
export class Deferred<T> extends Promise<T> {
  #resolve: (T) => Deferred<T> | void;
  #reject: (mixed) => Deferred<T> | void;

  constructor(
    fn?: (resolve?: (T) => mixed, reject?: (mixed) => mixed) => void
  ) {
    const proxy = {};
    super((resolve, reject) => {
      proxy.resolve = resolve;
      proxy.reject = reject;
      if (fn) fn(resolve, reject);
    });
    this.#resolve = proxy.resolve;
    this.#reject = proxy.reject;
  }

  resolve(result: T): Deferred<T> {
    this.#resolve(result);
    return this;
  }

  reject(error: mixed): Deferred<T> {
    this.#reject(error);
    return this;
  }
}
