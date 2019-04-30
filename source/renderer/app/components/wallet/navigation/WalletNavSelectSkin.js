// @flow
import React, { Component } from 'react';
import type { Element, Ref } from 'react';
import { Options } from 'react-polymorph/lib/components/Options';
import { OptionsSkin } from 'react-polymorph/lib/skins/simple/OptionsSkin';

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

export class SelectSkin extends Component<Props> {
  render() {
    const selectedOption = this.props.getSelectedOption();
    const { theme, themeId, ...rest } = this.props;

    return (
      <div ref={this.props.rootRef} className={theme[themeId].select}>
        {this.props.label}
        <Options
          skin={OptionsSkin}
          theme={theme}
          optionsRef={this.props.optionsRef}
          options={this.props.options}
          isOpeningUpward={this.props.isOpeningUpward}
          onChange={this.props.handleChange}
          optionRenderer={this.props.optionRenderer}
          selectedOption={selectedOption}
          noResults={!this.props.options.length}
          targetRef={this.props.inputRef}
          toggleOpen={this.props.toggleOpen}
          isOpen
          {...rest}
        />
      </div>
    );
  }
}
