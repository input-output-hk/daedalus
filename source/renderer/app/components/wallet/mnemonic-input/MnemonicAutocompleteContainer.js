'use strict';
// @ts-nocheck
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.MnemonicAutocompleteContainer = void 0;
const react_1 = __importStar(require('react'));
const fp_1 = require('lodash/fp');
const GlobalListeners_1 = require('@react-polymorph/components/HOC/GlobalListeners');
const MnemonicAutocompleteLayout_1 = require('./MnemonicAutocompleteLayout');
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
}) {
  const initialState = (0, react_1.useMemo)(
    () => ({
      inputValue: value,
      filteredOptions: options.slice(0, maxVisibleOptions),
      isOpen: false,
      mouseIsOverOptions: false,
      blurred: false,
    }),
    []
  );
  const [state, setState] = (0, react_1.useState)(initialState);
  const rootRef = (0, react_1.useRef)();
  const optionsRef = (0, react_1.useRef)();
  const suggestionsRef = (0, react_1.useRef)();
  const toggleOpen = () =>
    setState((prevState) => ({ ...prevState, isOpen: !prevState.isOpen }));
  const toggleMouseIsOverOptions = () =>
    setState((prevState) => ({
      ...prevState,
      mouseIsOverOptions: !prevState.mouseIsOverOptions,
    }));
  const handleInputClick = (event) => {
    toggleOpen();
    event.currentTarget.setSelectionRange(0, event.currentTarget.value.length);
  };
  const handleInputChange = (0, react_1.useCallback)(
    (event) => {
      const { value: inputValue } = event.currentTarget;
      let selectedOption = '';
      const trimmedInputValue = inputValue.trim();
      const filteredOptions = options
        .filter((0, fp_1.startsWith)(trimmedInputValue))
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
  const handleInputSelect = (0, react_1.useCallback)(
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
  (0, react_1.useEffect)(() => {
    if (!value) return;
    const filteredOptions = options
      .filter((0, fp_1.startsWith)(value))
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
  (0, react_1.useEffect)(() => {
    if (reset) {
      setState(initialState);
    }
  }, [reset]);
  return react_1.default.createElement(
    GlobalListeners_1.GlobalListeners,
    {
      mouseIsOverOptions: state.mouseIsOverOptions,
      optionsIsOpen: state.isOpen,
      optionsRef: optionsRef,
      rootRef: rootRef,
      toggleOpen: toggleOpen,
    },
    ({ optionsMaxHeight, optionHeight = 50 }) =>
      react_1.default.createElement(
        MnemonicAutocompleteLayout_1.MnemonicAutocompleteLayout,
        {
          ordinalNumber: ordinalNumber,
          value: state.inputValue,
          isOpen: state.isOpen,
          options: state.filteredOptions,
          optionsMaxHeight: optionsMaxHeight,
          noResultsMessage: noResultsMessage,
          rootRef: rootRef,
          inputRef: inputRef,
          optionsRef: optionsRef,
          onChange: handleInputChange,
          onSelect: handleInputSelect,
          onClick: handleInputClick,
          onBlur: handleBlur,
          onPaste: onPaste,
          suggestionsRef: suggestionsRef,
          toggleMouseLocation: toggleMouseIsOverOptions,
          toggleOpen: toggleOpen,
          optionHeight: optionHeight,
          disabled: disabled,
          error: state.blurred && state.inputValue && !value,
        }
      )
  );
}
exports.MnemonicAutocompleteContainer = MnemonicAutocompleteContainer;
//# sourceMappingURL=MnemonicAutocompleteContainer.js.map
