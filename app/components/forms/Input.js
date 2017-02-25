import React, { Component, PropTypes } from 'react';
import { omit } from 'lodash';

export default class Input extends Component {

  static propTypes = {
    skin: PropTypes.element.isRequired,
    onChange: PropTypes.func,
    onFocus: PropTypes.func,
    onBlur: PropTypes.func,
    value: PropTypes.string,
    disabled: PropTypes.bool,
    error: PropTypes.string,
  };

  static defaultProps = {
    value: '',
    disabled: false,
  };

  skinInputElement = null;

  registerSkinInputElement = (input) => this.skinInputElement = input;

  focus = () => this.skinInputElement && this.skinInputElement.focus();

  blur = () => this.skinInputElement && this.skinInputElement.blur();

  onChange = (event) => {
    const { onChange, disabled } = this.props;
    if (disabled) return;
    if(onChange) onChange(this._processValue(event.target.value), event);
  };

  render() {
    const { skin, value } = this.props;
    return React.cloneElement(skin, Object.assign({
      value: this._processValue(value),
      onChange: this.onChange,
      registerSkinInputElement: this.registerSkinInputElement,
    }, this._propsForSkin()));
  }

  _propsForSkin() {
    return omit(this.props, ['value', 'onChange']);
  }

  _processValue(value) {
    return value;
  }

}
