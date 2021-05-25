// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { Deferred } from '../../utils/Deferred';

const ABORT_ERROR = 'AbortError';

export class GraphQLRequest<TVariables, TResult> {
  @observable result: TResult | null = null;
  @observable isExecuting: boolean = false;
  @observable hasBeenExecutedAtLeastOnce: boolean = false;
  @observable error: Error | null = null;
  @observable execution: Deferred<TResult | null> | null = null;

  _request: (variables: TVariables) => Promise<TResult | null>;

  @computed get isExecutingTheFirstTime() {
    return this.isExecuting && !this.hasBeenExecutedAtLeastOnce;
  }

  constructor(request: (variables: TVariables) => Promise<TResult | null>) {
    this._request = request;
  }

  @action async execute(variables: TVariables): Promise<TResult | null> {
    if (this.isExecuting && this.execution != null) {
      this.execution.reject(ABORT_ERROR);
    }
    this.isExecuting = true;
    this.execution = new Deferred<TResult | null>((resolve, reject) => {
      this._request(variables).then(resolve).catch(reject);
    });
    try {
      const result = await this.execution;
      runInAction(() => {
        this.result = result;
        this.error = null;
      });
      return result;
    } catch (error) {
      if (error === ABORT_ERROR) {
        runInAction(() => {
          this.result = null;
          this.error = null;
        });
        return null;
      }
      runInAction(() => {
        this.result = null;
        this.error = error;
      });
      throw error;
    } finally {
      runInAction(() => {
        this.isExecuting = false;
        this.hasBeenExecutedAtLeastOnce = true;
      });
    }
  }
}
