import React, { Component, PropTypes } from 'react';
import { isFunction, isString, flow, pick } from 'lodash';
import Input from './Input';

export default class TextInput extends Input {

  static propTypes = Object.assign(Input.propTypes, {
    maxLength: PropTypes.number,
    onKeyPress: PropTypes.func,
  });

  _processValue(value) {
    return flow([
      this._enforceStringValue,
      this._enforceMaxLength
    ]).call(this, super._processValue(value));
  }

  _enforceStringValue(value) {
    if (!isString(value)) throw "Values passed to Input::onChange must be strings";
    return value;
  }

  _enforceMaxLength(value) {
    const { maxLength } = this.props;
    const isTooLong = maxLength != null && value.length > maxLength;
    return isTooLong ? value.substring(0, maxLength) : value;
  }
}
