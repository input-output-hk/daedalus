// @flow
import { observable, action } from 'mobx';
import { includes, camelCase, get, snakeCase } from 'lodash';
import { GenericApiError, messages as commonMessages } from '../api/common/errors';
import { messages } from '../api/errors';
import { logger } from '../utils/logging';

type LoggingType = {
  msg?: string,
  logError?: Object,
};

export default class ApiError {
  @observable tempError: string = '';
  @observable clause: boolean;
  @observable forceSet: boolean = false;
  isFinalError: boolean = false;
  id: string;
  defaultMessage: string;
  values: Object;
  code: string;

  constructor(error: Object = {}, logging?: LoggingType) {
    // Construct Localizable Error
    const errorCode = error.code ? camelCase(error.code) : null;
    const localizableError = get(messages, errorCode);
    let humanizedError;
    if (localizableError) {
      this.isFinalError = true;
      humanizedError = {
        id: localizableError.id,
        defaultMessage: localizableError.defaultMessage,
        values: error,
      };
    } else {
      const genericApiError = new GenericApiError(error);
      humanizedError = {
        id: genericApiError.id,
        defaultMessage: genericApiError.defaultMessage,
        values: genericApiError.values,
      };
    }
    Object.assign(this, {
      ...humanizedError,
      code: error.code,
    });

    // Set logging
    this._logError(logging);
  }

  @action set(predefinedError: string, force?: boolean = false) {
    if (predefinedError && !this.clause && (!this.isFinalError || (this.isFinalError && force))) {
      this.tempError = predefinedError;
      this.clause = true;
      this.forceSet = force;
    } else {
      this.clause = false;
    }
    return this;
  }

  @action where(type: string, declaration: string) {
    if (this.clause && !this.isFinalError) {
      this.clause = this.values[type] === declaration;
      if (!this.clause) {
        this.tempError = '';
      }
    }
    return this;
  }

  @action inc(type: string, declaration: string) {
    const fullMessage = get(this.values, type, '');
    if (this.clause && !this.isFinalError && fullMessage) {
      this.clause = includes(fullMessage, declaration);
      if (!this.clause) {
        this.tempError = '';
      }
    }
    return this;
  }

  @action result(fallbackError?: string) {
    if (this.isFinalError && !this.forceSet) return this;
    if (this.tempError && messages[this.tempError]) {
      Object.assign(this, {
        id: messages[this.tempError].id,
        defaultMessage: messages[this.tempError].defaultMessage,
        values: this.values,
        code: this.values.code,
      });
      return this;
    }
    if (fallbackError) {
      Object.assign(this, {
        id: messages[fallbackError].id,
        defaultMessage: messages[fallbackError].defaultMessage,
        values: this.values,
        code: snakeCase(fallbackError),
      });
      return this;
    }
    return new GenericApiError(this.values);
  }

  _logError(logging?: LoggingType) {
    if (logging && logging.msg) {
      const { logError, msg } = logging;
      logger.error(msg, { error: logError ? this.values : null });
    }
  }

  _getGenericApiError() {
    Object.assign(this, {
      id: commonMessages.genericApiError.id,
      defaultMessage: commonMessages.genericApiError.defaultMessage,
      values: this.values,
    });
    return this;
  }
}
