import React, { useCallback, useEffect, useRef, useState } from 'react';
import { startsWith } from 'lodash/fp';
import { GlobalListeners } from 'react-polymorph/lib/components/HOC/GlobalListeners';
import { MnemonicAutocompleteLayout } from './MnemonicAutocompleteLayout';

interface MnemonicsAutocompleteContainerProps {
  onChange: (value: string) => void;
  ordinalNumber: number;
  value: string;
  options: string[];
  maxVisibleOptions: number;
  disabled: boolean;
  error: boolean;
  reset: boolean;
  noResultsMessage: string;
}

const MnemonicsAutocompleteContainer = ({
  onChange,
  reset,
  ordinalNumber,
  value,
  options,
  maxVisibleOptions,
  disabled,
  error,
  noResultsMessage,
}: MnemonicsAutocompleteContainerProps) => {
  const [state, setState] = useState({
    inputValue: value,
    selectedOption: '',
    filteredOptions: options.slice(0, maxVisibleOptions - 1),
    isOpen: false,
    mouseIsOverOptions: false,
  });
  const rootRef = useRef();
  const inputRef = useRef();
  const optionsRef = useRef();
  const suggestionsRef = useRef();

  const toggleOpen = useCallback(() => {
    setState((prevState) => ({ ...prevState, isOpen: !prevState.isOpen }));
  }, []);

  const toggleMouseIsOverOptions = useCallback(() => {
    setState((prevState) => ({
      ...prevState,
      mouseIsOverOptions: !prevState.mouseIsOverOptions,
    }));
  }, []);

  const handleInputChange = useCallback(
    (inputValue) => {
      let selectedOption = '';

      if (options.includes(inputValue)) {
        selectedOption = inputValue;
      }

      setState((prevState) => ({
        ...prevState,
        isOpen: true,
        filteredOptions: options
          .filter(startsWith(inputValue))
          .slice(0, maxVisibleOptions - 1),
        inputValue,
        selectedOption,
      }));
    },
    [options]
  );

  const handleInputSelect = useCallback((inputValue) => {
    setState((prevState) => ({
      ...prevState,
      isOpen: false,
      filteredOptions: [inputValue],
      selectedOption: inputValue,
      inputValue,
    }));
  }, []);

  const [blurred, setBlurred] = useState(false);
  const handleBlur = useCallback(() => {
    setBlurred(true);
  }, [setBlurred]);

  useEffect(() => {
    onChange(state.selectedOption);
  }, [state.selectedOption]);

  useEffect(() => {
    if (reset) {
      setBlurred(false);
    }
  }, [reset]);

  useEffect(() => {
    if (value !== state.inputValue) {
      setState((prevState) => ({ ...prevState, inputValue: value }));
    }
  }, [value]);

  return (
    <GlobalListeners
      mouseIsOverOptions={state.mouseIsOverOptions}
      optionsIsOpen={state.isOpen}
      optionsRef={optionsRef}
      rootRef={rootRef}
      toggleOpen={toggleOpen}
    >
      {({ optionsMaxHeight, optionHeight = 50 }) => (
        <MnemonicAutocompleteLayout
          ordinalNumber={ordinalNumber}
          value={state.inputValue}
          isOpen={state.isOpen}
          options={state.filteredOptions}
          optionsMaxHeight={optionsMaxHeight}
          noResultsMessage={noResultsMessage}
          rootRef={rootRef}
          inputRef={inputRef}
          optionsRef={optionsRef}
          onChange={handleInputChange}
          onSelect={handleInputSelect}
          onClick={toggleOpen}
          onBlur={handleBlur}
          suggestionsRef={suggestionsRef}
          toggleMouseLocation={toggleMouseIsOverOptions}
          toggleOpen={toggleOpen}
          optionHeight={optionHeight}
          disabled={disabled}
          error={blurred && error}
        />
      )}
    </GlobalListeners>
  );
};

export { MnemonicsAutocompleteContainer };
