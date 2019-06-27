// @flow
import React, { Component } from 'react';
import type { Element, Ref } from 'react';
import classnames from 'classnames';
import { Options } from 'react-polymorph/lib/components/Options';
import { OptionsSkin } from 'react-polymorph/lib/skins/simple/OptionsSkin';
import optionsStyles from './DropdownOptionsStyles.scss';

type Props = {
  className: string,
  error: string | Element<any>,
  getSelectedOption: Function,
  handleChange: Function,
  handleInputClick: Function,
  inputRef: Ref<'input'>,
  isOpeningUpward: boolean,
  label: string | Element<any>,
  onBlur: Function,
  onChange: Function,
  onFocus: Function,
  options: Array<{
    isDisabled: boolean,
    value: any,
  }>,
  optionRenderer: Function,
  optionsRef: Ref<any>,
  placeholder: string,
  rootRef: Ref<*>,
  theme: Object, // will take precedence over theme in context if passed
  themeId: string,
  toggleOpen: Function,
  value: string,
};

export default class DropdownSelectSkin extends Component<Props> {
  optionRenderer = (option: any) => {
    const { theme, themeId } = this.props;
    const optionClassNames = classnames([
      theme[themeId].label,
      option.className,
    ]);

    return <span className={optionClassNames}>{option.label}</span>;
  };
  render() {
    const {
      rootRef,
      label,
      theme,
      themeId,
      getSelectedOption,
      handleChange,
      ...rest
    } = this.props;
    const selectedOption = getSelectedOption();

    return (
      <div ref={rootRef} className={theme[themeId].select}>
        {label}
        <Options
          skin={OptionsSkin}
          theme={theme}
          themeOverrides={optionsStyles}
          onChange={handleChange}
          optionRenderer={this.optionRenderer}
          selectedOption={selectedOption}
          value={selectedOption}
          noResults={!this.props.options.length}
          targetRef={this.props.inputRef}
          isOpen
          {...rest}
        />
      </div>
    );
  }
}
