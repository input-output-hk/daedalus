// @flow
import React, { Component } from 'react';
import type { Element, Ref } from 'react';

// external libraries
import classnames from 'classnames';

// components
import { Options } from 'react-polymorph/lib/components/Options';

// skins
import { OptionsSkin } from 'react-polymorph/lib/skins/simple/OptionsSkin';

type Props = {
  className: string,
  error: string | Element<any>,
  getSelectedOption: Function,
  handleChange: Function,
  handleInputClick: Function,
  inputRef: Ref<'input'>,
  // isOpen: boolean,
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

type State = {
  isOpen: boolean,
};

export class SelectSkin extends Component<Props, State> {
  state = {
    isOpen: false,
  };

  render() {
    const selectedOption = this.props.getSelectedOption();
    const { theme, themeId, ...rest } = this.props;

    return (
      <div
        ref={this.props.rootRef}
        className={classnames([
          this.props.className,
          theme[themeId].select,
          this.state.isOpen ? theme[themeId].isOpen : null,
          this.props.isOpeningUpward ? theme[themeId].openUpward : null,
        ])}
      >
        {this.props.label}
        <Options
          skin={OptionsSkin}
          theme={theme}
          isOpen
          optionsRef={this.props.optionsRef}
          options={this.props.options}
          isOpeningUpward={this.props.isOpeningUpward}
          onChange={this.props.handleChange}
          optionRenderer={this.props.optionRenderer}
          selectedOption={selectedOption}
          noResults={!this.props.options.length}
          targetRef={this.props.inputRef}
          toggleOpen={this.props.toggleOpen}
          {...rest}
        />
      </div>
    );
  }
}
