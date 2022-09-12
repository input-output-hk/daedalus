import React, {
  ChangeEventHandler,
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
  reset: boolean;
  noResultsMessage: string;
  inputRef: RefObject<HTMLInputElement>;
}

function MnemonicAutocompleteContainer({
  onChange,
  onConfirmSelection,
  onPaste,
  reset,
  ordinalNumber,
  value = '',
  options,
  maxVisibleOptions,
  disabled,
  noResultsMessage,
  inputRef,
}: MnemonicAutocompleteContainerProps) {
  const initialState = useMemo(
    () => ({
      inputValue: value,
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

  const toggleOpen = () =>
    setState((prevState) => ({ ...prevState, isOpen: !prevState.isOpen }));

  const toggleMouseIsOverOptions = () =>
    setState((prevState) => ({
      ...prevState,
      mouseIsOverOptions: !prevState.mouseIsOverOptions,
    }));

  const handleInputClick: MouseEventHandler<HTMLInputElement> = (event) => {
    toggleOpen();
    event.currentTarget.setSelectionRange(0, event.currentTarget.value.length);
  };

  const handleInputChange = useCallback<ChangeEventHandler<HTMLInputElement>>(
    (event) => {
      const { value: inputValue } = event.currentTarget;
      let selectedOption = '';
      const trimmedInputValue = inputValue.trim();
      const filteredOptions = options
        .filter(startsWith(trimmedInputValue))
        .slice(0, maxVisibleOptions);
      const isOpen =
        filteredOptions.length > 1 || filteredOptions[0] !== trimmedInputValue;

      if (filteredOptions.includes(trimmedInputValue)) {
        selectedOption = trimmedInputValue;
      }

      setState((prevState) => ({
        ...prevState,
        isOpen,
        filteredOptions,
        inputValue: trimmedInputValue,
      }));

      onChange(selectedOption);

      if (selectedOption && filteredOptions.length === 1) {
        onConfirmSelection();
      }
    },
    [options, onChange, onConfirmSelection, maxVisibleOptions]
  );

  const handleInputSelect = useCallback(
    (inputValue) => {
      setState((prevState) => ({
        ...prevState,
        isOpen: false,
        filteredOptions: [inputValue],
        inputValue,
      }));

      onChange(inputValue);
      onConfirmSelection();
    },
    [onChange, onConfirmSelection]
  );

  const handleBlur = () =>
    setState((prevState) => ({ ...prevState, blurred: true }));

  // this useEffect handles input paste event
  useEffect(() => {
    if (!value) return;

    const filteredOptions = options
      .filter(startsWith(value))
      .slice(0, maxVisibleOptions);

    if (filteredOptions.length > 1) {
      setState((prevState) => ({
        ...prevState,
        inputValue: value,
      }));
    } else {
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
          error={state.blurred && state.inputValue && !value}
        />
      )}
    </GlobalListeners>
  );
}

export { MnemonicAutocompleteContainer };
