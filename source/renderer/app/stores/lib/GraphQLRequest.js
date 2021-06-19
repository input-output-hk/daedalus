// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { Deferred } from '../../utils/Deferred';

const ABORT_ERROR = 'AbortError';

export class GraphQLRequest<TVariables, TResult> {
  @observable result: TResult | null = null;
  @observable isExecuting: boolean = false;
  @observable hasBeenExecutedAtLeastOnce: boolean = false;
  @observable error: Error | null = null;
  @observable execution: Deferred<TResult> | null = null;

  _request: (variables: TVariables) => Promise<TResult>;
  _wasRejectedForReExecution: boolean = false;

  @computed get isExecutingTheFirstTime() {
    return this.isExecuting && !this.hasBeenExecutedAtLeastOnce;
  }

  constructor(request: (variables: TVariables) => Promise<TResult>) {
    this._request = request;
  }

  @action async execute(variables: TVariables): Promise<TResult | null> {
    if (this.isExecuting && this.execution != null) {
      this._wasRejectedForReExecution = true;
      this.execution.reject(ABORT_ERROR);
    }
    this.execution = new Deferred<TResult>((resolve, reject) => {
      this._request(variables).then(resolve).catch(reject);
    });
    try {
      this.isExecuting = true;
      const result = await this.execution;
      runInAction(() => {
        this.result = result;
        this.error = null;
        this.isExecuting = false;
        this.hasBeenExecutedAtLeastOnce = true;
        this._wasRejectedForReExecution = false;
      });
      return result;
    } catch (error) {
      runInAction(() => {
        if (this._wasRejectedForReExecution) {
          this._wasRejectedForReExecution = false;
          return null;
        }
        this.isExecuting = false;
        this.result = null;
        if (error === ABORT_ERROR) {
          this.error = null;
        } else {
          this.error = error;
          this.hasBeenExecutedAtLeastOnce = true;
        }
      });
      return null;
    }
  }
}
