import React, {
  RefObject,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react';
import { startsWith } from 'lodash/fp';
import { GlobalListeners } from 'react-polymorph/lib/components/HOC/GlobalListeners';
import { MnemonicAutocompleteLayout } from './MnemonicAutocompleteLayout';

interface MnemonicAutocompleteContainerProps {
  onChange: (value: string) => void;
  onConfirmSelection: () => void;
  ordinalNumber: number;
  value: string;
  options: string[];
  maxVisibleOptions: number;
  disabled: boolean;
  error: boolean;
  reset: boolean;
  noResultsMessage: string;
  inputRef: RefObject<HTMLInputElement>;
}

const MnemonicAutocompleteContainer = ({
  onChange,
  onConfirmSelection,
  reset,
  ordinalNumber,
  value,
  options,
  maxVisibleOptions,
  disabled,
  error,
  noResultsMessage,
  inputRef,
}: MnemonicAutocompleteContainerProps) => {
  const initialState = useMemo(
    () => ({
      inputValue: value,
      selectedOption: '',
      filteredOptions: options.slice(0, maxVisibleOptions - 1),
      isOpen: false,
      mouseIsOverOptions: false,
      blurred: false,
    }),
    []
  );
  const [state, setState] = useState(initialState);
  const rootRef = useRef();
  const optionsRef = useRef();
  const suggestionsRef = useRef();

  const toggleOpen = useCallback(() => {
    if (disabled) return;
    setState((prevState) => ({ ...prevState, isOpen: !prevState.isOpen }));
  }, [disabled]);

  const toggleMouseIsOverOptions = useCallback(() => {
    setState((prevState) => ({
      ...prevState,
      mouseIsOverOptions: !prevState.mouseIsOverOptions,
    }));
  }, []);

  const handleInputChange = useCallback(
    (inputValue) => {
      if (disabled) return;
      let selectedOption = '';
      let isOpen = true;

      if (options.includes(inputValue)) {
        isOpen = false;
        selectedOption = inputValue;
      }

      setState((prevState) => ({
        ...prevState,
        isOpen,
        filteredOptions: options
          .filter(startsWith(inputValue.trim()))
          .slice(0, maxVisibleOptions - 1),
        inputValue,
        selectedOption,
      }));
    },
    [options, disabled]
  );

  const handleInputSelect = useCallback(
    (inputValue) => {
      if (disabled) return;
      setState((prevState) => ({
        ...prevState,
        isOpen: false,
        filteredOptions: [inputValue],
        selectedOption: inputValue,
        inputValue,
      }));
    },
    [disabled]
  );

  const handleBlur = useCallback(() => {
    if (disabled) return;
    setState((prevState) => ({ ...prevState, blurred: true }));
  }, [disabled]);

  useEffect(() => {
    onChange(state.selectedOption);
    if (state.selectedOption) {
      onConfirmSelection();
    }
  }, [state.selectedOption]);

  useEffect(() => {
    if (reset) {
      setState(initialState);
    }
  }, [reset]);

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
          error={state.blurred && error}
        />
      )}
    </GlobalListeners>
  );
};

export { MnemonicAutocompleteContainer };
