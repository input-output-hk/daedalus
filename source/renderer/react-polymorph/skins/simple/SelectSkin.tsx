// @ts-nocheck
import React from 'react';
import type { Element, ElementRef } from 'react';
// external libraries
import classnames from 'classnames';
// components
import { Options } from '../../components/Options';
import { Input } from '../../components/Input';
// skins
import { OptionsSkin } from './OptionsSkin';
import { InputSkin } from './InputSkin';

type Props = {
  className: string;
  disabled?: boolean;
  error: string | Element<any>;
  getSelectedOption: (...args: Array<any>) => any;
  handleChange: (...args: Array<any>) => any;
  handleInputClick: (...args: Array<any>) => any;
  hasSearch?: boolean;
  hideSearchClearButton?: boolean;
  highlightSearch?: boolean;
  inputRef: ElementRef<'input'>;
  isOpen: boolean;
  isOpeningUpward: boolean;
  label: string | Element<any>;
  noResultsMessage?: string;
  onBlur: (...args: Array<any>) => any;
  onChange: (...args: Array<any>) => any;
  onFocus: (...args: Array<any>) => any;
  onSearch?: (...args: Array<any>) => any;
  options: Array<{
    isDisabled: boolean;
    value: any;
  }>;
  optionRenderer: (...args: Array<any>) => any;
  optionsRef: ElementRef<any>;
  optionsMaxHeight: number;
  placeholder: string;
  rootRef: ElementRef<any>;
  selectionRenderer?: (...args: Array<any>) => any;
  theme: Record<string, any>;
  // will take precedence over theme in context if passed
  themeId: string;
  toggleOpen: (...args: Array<any>) => any;
  toggleMouseLocation: (...args: Array<any>) => any;
  value: string;
  optionHeight: number | null | undefined;
  searchHeight: number | null | undefined;
};
export function SelectSkin(props: Props) {
  const selectedOption = props.getSelectedOption();
  const inputValue = selectedOption ? selectedOption.label : '';
  const { theme, themeId } = props;
  return (
    <div
      ref={props.rootRef}
      className={classnames(
        props.className,
        theme[themeId].select,
        props.isOpen && theme[themeId].isOpen,
        props.isOpeningUpward && theme[themeId].openUpward,
        props.disabled && theme[themeId].disabled
      )}
    >
      <div className={theme[themeId].selectInput}>
        <Input
          skin={InputSkin}
          theme={theme}
          inputRef={props.inputRef}
          label={props.label}
          value={inputValue}
          onClick={props.handleInputClick}
          placeholder={props.placeholder}
          error={props.error}
          selectionRenderer={props.selectionRenderer}
          readOnly
          disabled={props.disabled}
          selectedOption={selectedOption}
        />
      </div>
      <Options
        skin={OptionsSkin}
        theme={theme}
        hasSearch={props.hasSearch}
        hideSearchClearButton={props.hideSearchClearButton}
        highlightSearch={props.highlightSearch}
        isOpen={props.isOpen}
        optionsRef={props.optionsRef}
        optionsMaxHeight={props.optionsMaxHeight}
        options={props.options}
        isOpeningUpward={props.isOpeningUpward}
        onChange={props.handleChange}
        onSearch={props.onSearch}
        optionRenderer={props.optionRenderer}
        selectedOption={selectedOption}
        noResults={!props.options.length}
        targetRef={props.inputRef}
        toggleMouseLocation={props.toggleMouseLocation}
        toggleOpen={props.toggleOpen}
        optionHeight={props.optionHeight}
        searchHeight={props.searchHeight}
      />
    </div>
  );
}
