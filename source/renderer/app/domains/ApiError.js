// @flow
import { observable, action } from 'mobx';
import { includes, camelCase, get, snakeCase, map, keys, omit } from 'lodash';
import { GenericApiError } from '../api/common/errors';
import { messages } from '../api/errors';
import { logger } from '../utils/logging';

type KnownErrorType =
  | 'invalid_wallet_type'
  | 'no_such_wallet'
  | 'no_such_transaction'
  | 'transaction_not_pending'
  | 'wallet_already_exists'
  | 'no_root_key'
  | 'wrong_encryption_passphrase'
  | 'malformed_tx_payload'
  | 'key_not_found_for_address'
  | 'not_enough_money'
  | 'utxo_not_enough_fragmented'
  | 'transaction_is_too_big'
  | 'inputs_depleted'
  | 'cannot_cover_fee'
  | 'invalid_coin_selection'
  | 'network_unreachable'
  | 'network_misconfigured'
  | 'network_tip_not_found'
  | 'created_invalid_transaction'
  | 'rejected_by_core_node'
  | 'bad_request'
  | 'not_found'
  | 'method_not_allowed'
  | 'not_acceptable'
  | 'start_time_later_than_end_time'
  | 'unsupported_media_type'
  | 'unexpected_error'
  | 'not_synced'
  | 'nothing_to_migrate'
  | 'no_such_pool'
  | 'pool_already_joined'
  | 'not_delegating_to'
  | 'invalid_restoration_parameters'
  | 'rejected_tip'
  | 'no_such_epoch_no'
  | 'invalid_delegation_discovery'
  | 'not_implemented'
  | 'wallet_not_responding'
  | 'address_already_exists';

type LoggingType = {
  msg?: string,
  logError?: Object,
};

type ErrorType = {
  code?: KnownErrorType,
  message?: string,
};

export default class ApiError {
  @observable tempError: string = '';
  @observable clause: boolean;
  @observable forceSet: boolean = false;
  @observable additionalValues: Object = {};
  isFinalError: boolean = false;
  id: string;
  defaultMessage: string;
  values: Object;
  code: string;

  constructor(error: ErrorType = {}, logging?: LoggingType) {
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

  @action set(
    predefinedError: string,
    force?: boolean = false,
    values?: Object
  ) {
    if (
      predefinedError &&
      !this.clause &&
      (!this.isFinalError || (this.isFinalError && force))
    ) {
      this.tempError = predefinedError;
      this.clause = true;
      this.forceSet = force;
    } else {
      this.clause = false;
    }

    if (values && this.clause) {
      const transformedValues = {};
      map(values, (val, key) => {
        const translated = get(messages, val);
        if (translated) {
          transformedValues[key] = translated;
        } else {
          transformedValues[key] = val;
        }
      });
      this.additionalValues = transformedValues;
      Object.assign(this, {
        values: {
          ...this.values,
          ...transformedValues,
        },
      });
    }
    return this;
  }

  @action where(type: string, declaration: string) {
    if (
      this.clause &&
      (!this.isFinalError || (this.isFinalError && this.forceSet))
    ) {
      this.clause = this.values[type] === declaration;
      if (!this.clause) {
        this.tempError = '';
        if (this.additionalValues) {
          const additionalValuesKeys = keys(this.additionalValues);
          this.values = omit(this.values, additionalValuesKeys);
        }
        if (this.forceSet) {
          this.clause = false;
          this.forceSet = false;
        }
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
}
