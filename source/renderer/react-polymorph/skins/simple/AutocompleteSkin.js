// @flow
import React from 'react';
import type { ElementRef } from 'react';

// external libraries
import { slice, some } from 'lodash';
import classnames from 'classnames';
import type { AutocompleteProps } from '../../components/Autocomplete';

// components
import { FormField } from '../../components/FormField';
import { Options } from '../../components/Options';

// skins
import { OptionsSkin } from './OptionsSkin';

type Props = AutocompleteProps & {
  filteredOptions: Array<any>,
  getSelectionProps: Function,
  handleAutocompleteClick: Function,
  handleChange: Function,
  handleInputChange: Function,
  inputRef: ElementRef<any>,
  inputValue: string,
  isOpen: boolean,
  onKeyDown: Function,
  optionsRef: ElementRef<any>,
  optionsMaxHeight: number,
  removeOption: Function,
  rootRef: ElementRef<any>,
  selectedOptions: Array<any>,
  suggestionsRef: ElementRef<any>,
  toggleMouseLocation: Function,
  toggleOpen: Function,
  optionHeight: ?number,
};

export function AutocompleteSkin(props: Props) {
  const theme = props.theme[props.themeId];

  const filteredAndLimitedOptions = slice(
    props.filteredOptions,
    0,
    props.maxVisibleOptions
  );

  // show placeholder only if no maximum selections declared or maximum not reached
  const canMoreOptionsBeSelected =
    props.selectedOptions.length < props.maxSelections;

  const placeholder =
    !props.maxSelections || canMoreOptionsBeSelected ? props.placeholder : '';

  const renderSelectedOptions = () => {
    // check if the user passed a renderSelections function
    if (props.selectedOptions && props.renderSelections) {
      // call custom renderSelections function
      return props.renderSelections(props.getSelectionProps);
    }
    if (props.selectedOptions && !props.renderSelections) {
      // render default skin
      return props.selectedOptions.map((selectedOption, index) => (
        <span className={theme.selectedWordBox} key={index}>
          <span className={theme.selectedWordValue}>
            {selectedOption}
            <span
              role="presentation"
              aria-hidden
              className={theme.selectedWordRemoveButton}
              onClick={props.removeOption.bind(null, index)}
            >
              &times;
            </span>
          </span>
        </span>
      ));
    }
    return null;
  };

  const selectedOptionsCount = props.selectedOptions.length;
  const hasSelectedRequiredNumberOfOptions =
    props.requiredSelections.length === 0 ||
    some(
      props.requiredSelections,
      (requiredCount) => selectedOptionsCount === requiredCount
    );
  const error = hasSelectedRequiredNumberOfOptions ? props.error : null;

  return (
    <div
      aria-hidden
      className={classnames([props.className, theme.autocompleteWrapper])}
      onClick={props.handleAutocompleteClick}
      ref={props.rootRef}
      role="presentation"
    >
      {props.requiredSelections.length > 0 &&
        props.requiredSelectionsInfo != null && (
          <div className={theme.requiredWordsInfo}>
            {props.requiredSelectionsInfo(
              props.requiredSelections,
              selectedOptionsCount
            )}
          </div>
        )}
      <FormField
        error={error}
        formFieldRef={props.inputRef}
        label={props.label}
        isErrorHidden={props.isOpen}
        render={(setFormFieldRef) => (
          <div
            className={classnames([
              theme.autocompleteContent,
              props.isOpen ? theme.opened : null,
              props.selectedOptions.length ? theme.hasSelectedWords : null,
              error ? theme.errored : null,
            ])}
            ref={props.suggestionsRef}
          >
            <div className={theme.selectedWords}>
              {renderSelectedOptions()}
              <input
                ref={setFormFieldRef}
                placeholder={placeholder}
                value={props.inputValue}
                onChange={props.handleInputChange}
                onKeyDown={props.onKeyDown}
              />
            </div>
          </div>
        )}
      />

      <Options
        isOpen={props.isOpen}
        isOpeningUpward={props.isOpeningUpward}
        noResults={!props.filteredOptions.length}
        onChange={props.handleChange}
        options={filteredAndLimitedOptions}
        optionsRef={props.optionsRef}
        optionsMaxHeight={props.optionsMaxHeight}
        render={props.renderOptions}
        resetOnClose
        selectedOptions={props.selectedOptions}
        skin={OptionsSkin}
        targetRef={props.suggestionsRef}
        toggleMouseLocation={props.toggleMouseLocation}
        toggleOpen={props.toggleOpen}
        optionHeight={props.optionHeight}
      />
    </div>
  );
}
