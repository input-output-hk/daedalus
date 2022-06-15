import React, {
  ClipboardEventHandler,
  MouseEventHandler,
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
  onPaste: ClipboardEventHandler<HTMLInputElement>;
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
  onPaste,
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
      filteredOptions: options.slice(0, maxVisibleOptions),
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

  const handleInputClick: MouseEventHandler<HTMLInputElement> = (event) => {
    if (disabled) return;
    toggleOpen();
    event.currentTarget.setSelectionRange(0, event.currentTarget.value.length);
  };

  const handleInputChange = useCallback(
    (inputValue) => {
      if (disabled) return;
      let selectedOption = '';
      let isOpen = true;
      const trimmedInputValue = inputValue.trim();

      if (options.includes(trimmedInputValue)) {
        isOpen = false;
        selectedOption = trimmedInputValue;
      }

      setState((prevState) => ({
        ...prevState,
        isOpen,
        filteredOptions: options
          .filter(startsWith(trimmedInputValue))
          .slice(0, maxVisibleOptions),
        inputValue: trimmedInputValue,
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
    if (value) {
      handleInputSelect(value);
    }
  }, [value]);

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
          onClick={handleInputClick}
          onBlur={handleBlur}
          onPaste={onPaste}
          suggestionsRef={suggestionsRef}
          toggleMouseLocation={toggleMouseIsOverOptions}
          toggleOpen={toggleOpen}
          optionHeight={optionHeight}
          disabled={disabled}
          error={state.blurred && state.inputValue && error}
        />
      )}
    </GlobalListeners>
  );
};

export { MnemonicAutocompleteContainer };
